%%%-------------------------------------------------------------------
%%% File:      erlyjs_demo.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc       ErlyJS demo application
%%% @reference  See <a href="http://erlyjs.googlecode.com" target="_top">http://erlyjs.googlecode.com</a> for more information
%%% @end
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
%%% @since 2007-12-14 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlyjs_demo).
-author('rsaccon@gmail.com').

%% API
-export([start/0, test/0, test/1]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc
%% @end 
%%--------------------------------------------------------------------
start() ->
    erlyjs:start_link().
    
    
%%--------------------------------------------------------------------
%% @spec () -> Ast::tuple()
%% @doc
%% @end 
%%--------------------------------------------------------------------
test() ->        
    Dir = filename:join([filename:dirname(code:which(?MODULE)),"..", "demo"]),
    test(filename:join([Dir, "test.js"])).

 
%%--------------------------------------------------------------------
%% @spec (string()) -> Ast::tuple()
%% @doc
%% @end 
%%--------------------------------------------------------------------
test(File) ->   
	case file:read_file(File) of
        {ok, B} ->
            ErlAst = erlyjs:transl(erlyjs:ast(B)),
            io:format("TRACE ~p:~p Erlang AST:~n ~p~n",[?MODULE, ?LINE, ErlAst]);
		_ ->
		    io:format("TRACE ~p:~p reading js sourcefile failed~n",[?MODULE, ?LINE])
	end.
	
%%====================================================================
%% Internal functions
%%====================================================================

