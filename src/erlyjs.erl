%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright  2007 Roberto Saccon
%%% @doc        Javascript to Erlang compiler
%%% @reference  See <a href="http://erlyjs.googlecode.com" target="_top">http://erlyjs.googlecode.com</a> for more information
%%% @end
%%%
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
%%%---------------------------------------------------------------------------------------
%%
%% EOF = 0,                        /* end of file */
%% EOL = 1,                        /* end of line */
%% SEMI = 2,                       /* semicolon */
%% COMMA = 3,                      /* comma operator */
%% ASSIGN = 4,                     /* assignment ops (= += -= etc.) */
%% HOOK = 5, COLON = 6,            /* conditional (?:) */
%% OR = 7,                         /* logical or (||) */
%% AND = 8,                        /* logical and (&&) */
%% BITOR = 9,                      /* bitwise-or (|) */
%% BITXOR = 10,                    /* bitwise-xor (^) */
%% BITAND = 11,                    /* bitwise-and (&) */
%% EQOP = 12,                      /* equality ops (== !=) */
%% RELOP = 13,                     /* relational ops (< <= > >=) */
%% SHOP = 14,                      /* shift ops (<< >> >>>) */
%% PLUS = 15,                      /* plus */
%% MINUS = 16,                     /* minus */
%% STAR = 17, DIVOP = 18,          /* multiply/divide ops (* / %) */
%% UNARYOP = 19,                   /* unary prefix operator */
%% INC = 20, DEC = 21,             /* increment/decrement (++ --) */
%% DOT = 22,                       /* member operator (.) */
%% LB = 23, RB = 24,               /* left and right brackets */
%% LC = 25, RC = 26,               /* left and right curlies (braces) */
%% LP = 27, RP = 28,               /* left and right parentheses */
%% NAME = 29,                      /* identifier */
%% NUMBER = 30,                    /* numeric constant */
%% STRING = 31,                    /* string constant */
%% REGEXP = 32,                    /* RegExp constant */
%% PRIMARY = 33,                   /* true, false, null, this, super */
%% FUNCTION = 34,                  /* function keyword */
%% EXPORT = 35,                    /* export keyword */
%% IMPORT = 36,                    /* import keyword */
%% IF = 37,                        /* if keyword */
%% ELSE = 38,                      /* else keyword */
%% SWITCH = 39,                    /* switch keyword */
%% CASE = 40,                      /* case keyword */
%% DEFAULT = 41,                   /* default keyword */
%% WHILE = 42,                     /* while keyword */
%% DO = 43,                        /* do keyword */
%% FOR = 44,                       /* for keyword */
%% BREAK = 45,                     /* break keyword */
%% CONTINUE = 46,                  /* continue keyword */
%% IN = 47,                        /* in keyword */
%% VAR = 48,                       /* var keyword */
%% WITH = 49,                      /* with keyword */
%% RETURN = 50,                    /* return keyword */
%% NEW = 51,                       /* new keyword */
%% DELETE = 52,                    /* delete keyword */
%% DEFSHARP = 53,                  /* #n= for object/array initializers */
%% USESHARP = 54,                  /* #n# for object/array initializers */
%% TRY = 55,                       /* try keyword */
%% CATCH = 56,                     /* catch keyword */
%% FINALLY = 57,                   /* finally keyword */
%% THROW = 58,                     /* throw keyword */
%% INSTANCEOF = 59,                /* instanceof keyword */
%% DEBUGGER = 60,                  /* debugger keyword */
%% XMLSTAGO = 61,                  /* XML start tag open (<) */
%% XMLETAGO = 62,                  /* XML end tag open (</) */
%% XMLPTAGC = 63,                  /* XML point tag close (/>) */
%% XMLTAGC = 64,                   /* XML start or end tag close (>) */
%% XMLNAME = 65,                   /* XML start-tag non-final fragment */
%% XMLATTR = 66,                   /* XML quoted attribute value */
%% XMLSPACE = 67,                  /* XML whitespace */
%% XMLTEXT = 68,                   /* XML text */
%% XMLCOMMENT = 69,                /* XML comment */
%% XMLCDATA = 70,                  /* XML CDATA section */
%% XMLPI = 71,                     /* XML processing instruction */
%% AT = 72,                        /* XML attribute op (@) */
%% DBLCOLON = 73,                  /* namespace qualified name op (::) */
%% ANYNAME = 74,                   /* XML AnyName singleton (*) */
%% DBLDOT = 75,                    /* XML descendant op (..) */
%% FILTER = 76,                    /* XML filtering predicate op (.()) */
%% XMLELEM = 77,                   /* XML element node type (no token) */
%% XMLLIST = 78,                   /* XML list node type (no token) */
%% YIELD = 79,                     /* yield from generator function */
%% ARRAYCOMP = 80,                 /* array comprehension initialiser */
%% ARRAYPUSH = 81,                 /* array push within comprehension */
%% LEXICALSCOPE = 82,              /* block scope AST node label */
%% LET = 83,                       /* let keyword */
%% BODY = 84,                      /* synthetic body of function with
%%                                    destructuring formal parameters */
%% RESERVED,                       /* reserved keyword */
%% LIMIT                           /* domain size */
%%----------------------------------------------------------------------------------------
-module(erlyjs).
-author('rsaccon@gmail.com').

-behaviour(gen_server).

-define(DEFAULT_CNODE_NUMBER, 2).
-define(TIMEOUT, 1000).

-export([start_link/0, start_link/1, stop/0, ast/1, transl/1]).
	
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {cnode,
                port}).

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
     
            
%%--------------------------------------------------------------------
%% @spec (Source::binary()) ->
%%     (ok | {error, Reason})
%% @doc
%% Creates a blank image.
%% @end 
%%--------------------------------------------------------------------
ast(Binary) ->
    gen_server:call(?MODULE, {list_to_atom(binary_to_list(Binary))}).	

    
%%--------------------------------------------------------------------
%% @spec (tuple()) -> tuple()
%% @doc
%% translates JS AST to Erlang AST
%% @end 
%%--------------------------------------------------------------------
transl({error, Reason}) ->
    io:format("TRACE ~p:~p Error: ~p~n",[?MODULE, ?LINE, Reason]);

transl({{'LC', _Loc}, List}) ->
	[transl(X) || X <- lists:reverse(List)];

transl({{'NAME', _Loc}, Val}) -> 
	{var, 1, Val};

transl(Other) ->
    io:format("TRACE ~p:~p Other: ~p~n",[?MODULE, ?LINE, Other]),
	Other.

    
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
    {ok, #state{cnode = list_to_atom(CNodeName), port=Port}}.

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
    call_cnode(State#state.cnode, {stop, {}}),
    {stop, normal, State};

handle_call(Msg, _From, State) ->
    Reply = call_cnode(State#state.cnode, Msg),
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

call_cnode(CNode, Msg) ->
    {any, CNode} ! {call, self(), Msg},
    receive
        %% TODO: receive only from our CNode with specific CNodeNumber
        %% {cnode, CNodeNumber, Result} ->
        %%     Result
        Result ->
            Result        
    after 
        ?TIMEOUT ->
            %% TODO: proper errorlogging
            {error, timeout}
    end.    