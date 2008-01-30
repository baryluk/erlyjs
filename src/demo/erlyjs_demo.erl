%%%-------------------------------------------------------------------
%%% File:      erlyjs_demo.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc       ErlyJS demo application
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
-export([compile/0, test/1]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec () -> Ast::tuple()
%% @doc
%% @end 
%%--------------------------------------------------------------------
compile() ->        
    Dir = filename:join([filename:dirname(code:which(?MODULE)),"..", "demo"]),
    test(filename:join([Dir, "test.js"])).

 
%%--------------------------------------------------------------------
%% @spec (string()) -> Ast::tuple()
%% @doc
%% @end 
%%--------------------------------------------------------------------
test(File) ->   
	Module = filename:rootname(filename:basename(File)),
    erlyjs_compiler:compile(File, Module).
	
%%====================================================================
%% Internal functions
%%====================================================================