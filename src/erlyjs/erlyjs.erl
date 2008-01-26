%%%-------------------------------------------------------------------
%%% File:      erlyjs.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc  
%%% Helper module to start and stop ErlyJS application
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
%%% @since 2007-11-17 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlyjs).
-author('rsaccon@gmail.com').

    
%% API
-export([create_lexer/0, create_parser/0]).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc creates the yecc-based ErlyJS parser
%% @end 
%%--------------------------------------------------------------------
create_parser() ->
    create_parser("src/erlyjs/erlyjs_parser", "ebin").
        
create_lexer() ->
    create_lexer("src/erlyjs/erlyjs_lexer", "ebin").


%%====================================================================
%% Internal functions
%%====================================================================

create_lexer(Path, Outdir) ->
    case leex:file(Path) of
        ok ->
            compile(Path, Outdir, "Lexer");
        _ ->
            {error, "leexer generation failed"}
    end.

    
create_parser(Path, Outdir) ->
    case yecc:file(Path) of
        {ok, _} ->
            compile(Path, Outdir, "Parser");
        _ ->
            {error, "parser generation failed"}
    end.


compile(Path, Outdir, Name) ->
    case compile:file(Path, [{outdir, Outdir}]) of
        {ok, Bin} ->
            code:purge(Bin),
            case code:load_file(Bin) of
                {module, _} ->
                    ok;
                _ ->
                    {error, Name ++ "reload failed"}
            end;
        _ ->
            {error, Name ++ "compilation failed"}
    end.