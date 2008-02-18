%%%-------------------------------------------------------------------
%%% File:      erlyjs_operators.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2008 Roberto Saccon
%%% @doc  
%%% Logical and binary Operators
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2008 Roberto Saccon
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
%%% @since 2008-02-18 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlyjs_operators).
-author('rsaccon@gmail.com').

%% API
-export([ast/2, ast/3, ast/4]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec 
%% @doc
%% @end 
%%--------------------------------------------------------------------
ast('++', Ast) ->
    %% TODO: dynamic typechecking and implemenntation
    erl_syntax:infix_expr(Ast, erl_syntax:operator('+'), erl_syntax:integer(1));
ast('--', Ast) ->
    %% TODO: dynamic typechecking and implemenntation
    erl_syntax:infix_expr(Ast, erl_syntax:operator('-'), erl_syntax:integer(1));
ast('-' = Op, Ast) ->
    %% TODO: dynamic typechecking and implemenntation
    erl_syntax:infix_expr(erl_syntax:integer(0), erl_syntax:operator(Op), Ast);
ast('~', Ast) ->
    %% TODO: dynamic typechecking and implemenntation
    erl_syntax:prefix_expr(erl_syntax:operator('bnot'), Ast);   
ast('!', Ast) ->
    erl_syntax:case_expr(Ast, [
        erl_syntax:clause([erl_syntax:atom(false)], none, [erl_syntax:atom(true)]),
        erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(false)])]).
           
           
ast('*' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
ast('/' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
ast('%', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('rem'), Ast2);    
ast('+' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
ast('-' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
ast('<<', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator("bsl"), Ast2);
ast('>>', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator("bsr"), Ast2);  
ast('>>>', _Ast1, _Ast2) ->
    %% TODO: implementation and dynamic typechecking
    %% right-shift 9 with 2 
    %% <<Val:30, Ignore:2>> = <<9:32>>.
    %% Result = <<0:2, Val:30>>.  
    %% how do we handle negatie numbers (two complement format) ?
    erl_syntax:atom(not_implemented_yet);    
ast('<' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
ast('>' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
ast('<=', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('=<'), Ast2);
ast('>=' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
ast('==' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
ast('!=', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('/='), Ast2);
ast('===', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('=:='), Ast2);
ast('!==', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('=/='), Ast2);
ast('&', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('band'), Ast2);
ast('^', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('bxor'), Ast2);
ast('|' , Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('bor'), Ast2);
ast('&&', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('and'), Ast2);
ast('||', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('or'), Ast2);
ast(Unknown, _, _) ->    
    throw({error, lists:concat(["Unknown operator: ", Unknown])}).
  
    
ast('cond', Ast1, Ast2, Ast3) ->
    %% TODO: dynamic typechecking    
    erl_syntax:case_expr(Ast1, [
        erl_syntax:clause([erl_syntax:atom(true)], none, [Ast2]),
        erl_syntax:clause([erl_syntax:underscore()], none, [Ast3])]);
ast(Unknown, _, _, _) ->    
    throw({error, lists:concat(["Unknown operator: ", Unknown])}).
%%====================================================================
%% Internal functions
%%====================================================================

