%%%-------------------------------------------------------------------
%%% File:      erlycairo_server.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc  
%%% Javascript to Erlang compiler implemented as c-node
%%% @end
%%% @reference  See <a href="http://erlycairo.googlecode.com" target="_top">http://erlycairo.googlecode.com</a> for more information  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-11-29 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlyjs_server).
-author('rsaccon@gmail.com').

-behaviour(gen_server).

-define(DEFAULT_CNODE_NUMBER, 2).
-define(TIMEOUT, 1000).

%% API
-export([start_link/0, 
    start_link/1,
    stop/0,
    compile/2,
    compile/3,
    compile/4,
    set_var/2,
    get_var/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {cnode, cnode_number, port}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec () -> {ok,Pid} | ignore | {error,Error}
%% @doc 
%% Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ?DEFAULT_CNODE_NUMBER, []).
    
    
%%--------------------------------------------------------------------
%% @spec (CNodeNumber::integer()) -> {ok,Pid} | ignore | {error,Error}
%% @doc 
%% Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link(CNodeNumber) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, CNodeNumber, []).
  
  
%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc 
%% Stops the server
%% @end 
%%--------------------------------------------------------------------  
stop() ->
    gen_server:call(?MODULE, stop).
     

compile(Bin, Mod) when is_binary(Bin) ->
    compile(Bin, Mod, "run").
  
compile(Bin, _, [$f, $u, $n, $c, $_ | _]) when is_binary(Bin) ->
    {error, "'func_' not allowed in global function name"};   
    
compile(Bin, Mod, Func) when is_binary(Bin) ->
    compile(Bin, Mod, Func, "ebin").    
 
    
set_var(_, _) ->
    ok.
    
get_var(_) ->
    ok.    
                
%%--------------------------------------------------------------------
%% @spec (JsFile::binray()) -> (ok | {error, Reason})
%% @doc
%% compiles a Javascript document.
%% @end 
%%--------------------------------------------------------------------
compile(Bin, Mod, Func, OutDir) when is_binary(Bin) ->
    case gen_server:call(?MODULE, {c_node, {list_to_atom(binary_to_list(Bin))}}) of
        {ok, Val} ->
            io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Val]),
            erlyjs_compiler:compile(Val, Mod, Func, OutDir);
        Err ->
            Err
    end.
        
        
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server
%% @end 
%%--------------------------------------------------------------------  
init(CNodeNumber) ->  
    CNodeBinPath = filename:join([filename:dirname(code:which(?MODULE)),"..", "priv", "bin", "erlyjs"]),
    Cookie = atom_to_list(erlang:get_cookie()),
    Node = atom_to_list(node()),
    Cmd = lists:concat([CNodeBinPath, " ", CNodeNumber, " ", Cookie, " ", Node]),
    Port = open_port({spawn, Cmd}, []),   
    HostName = string:strip(os:cmd("hostname -s"), right, $\n),
    CNodeName = lists:concat(["c", CNodeNumber, "@", HostName]),
    {ok, #state{cnode_number = CNodeNumber, cnode = list_to_atom(CNodeName), port=Port}}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    call_cnode(State#state.cnode, State#state.cnode_number, {stop, {}}),
    {stop, normal, State};
    
handle_call({c_node, Msg}, _From, State) ->
    Reply = call_cnode(State#state.cnode, State#state.cnode_number, Msg),
    {reply, Reply, State}.
 

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, "terminate"]),
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

call_cnode(CNode, CNodeNumber, Msg) ->    
    {any, CNode} ! {call, self(), Msg},
    receive      
        {c_node, CNodeNumber, Result} ->
            {ok, Result}
    after 
        ?TIMEOUT ->
            %% TODO: proper errorlogging
            {error, timeout}
    end.