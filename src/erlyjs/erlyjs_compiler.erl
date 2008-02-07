%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright  2007 Roberto Saccon
%%% @doc        Javascript to Erlang compiler
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
-module(erlyjs_compiler).
-author('rsaccon@gmail.com').

%% API
-export([compile/2, compile/3]).


-record(js_context, {
    out_dir = "ebin",
    global = true,   
    args = [],
    api_namespace = [],
    reader = {file, read_file},
    action = get,
    force_recompile = false,
    module = [],
    verbose = false}).
    
-record(ast_info, {
    vars = [],
    export_asts = [],
    global_asts = []}).

-record(scope, {
    names = [],
    names_used_set = sets:new(),
    names_dict = dict:new()}).
 
 
compile(File, Module) ->
    compile(File, Module, []).
     
compile(File, Module, Options) ->
    Context = init_js_context(File, Module, Options),
    {M, F} = Context#js_context.reader,
    case catch M:F(File) of
        {ok, Data} ->
            crypto:start(),
            CheckSum = binary_to_list(crypto:sha(Data)),
            case parse(CheckSum, Data, Context) of    
                ok ->
                    ok;
                {ok, JsParseTree} ->
                    trace(?MODULE, ?LINE, "JsParseTree", JsParseTree, Context),
                    try body_ast(JsParseTree, Context, [#scope{}]) of
                        {AstList, Info, _} ->                
                            Forms = forms(CheckSum, Module, AstList, Info),  
                            trace(?MODULE, ?LINE, "Forms", Forms, Context),
                            compile_forms(Forms, Context)                                      
                    catch 
                        throw:Error -> Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            {error, "reading " ++ File ++ " failed "}
    end.
  		
	
%%====================================================================
%% Internal functions
%%====================================================================    

init_js_context(_File, Module, Options) ->
    Ctx = #js_context{},
    #js_context{ 
        module = list_to_atom(Module),
        out_dir = proplists:get_value(out_dir, Options,  Ctx#js_context.out_dir),
        verbose = proplists:get_value(verbose, Options, Ctx#js_context.verbose),
        reader = proplists:get_value(reader, Options, Ctx#js_context.reader),
        force_recompile = proplists:get_value(force_recompile, Options, Ctx#js_context.force_recompile)}.
      
        
parse(Data) ->
    case erlyjs_lexer:string(binary_to_list(Data)) of
        {ok, Tokens, _} ->
            erlyjs_parser:parse(Tokens);
        Err ->
            Err
    end.
        

parse(_, Data, #js_context{force_recompile = true}) ->  
    parse(Data);
parse(CheckSum, Data, Context) ->
    Module = Context#js_context.module,
    case catch Module:checksum() of
        CheckSum ->   
            ok;
        _ ->
            parse(Data)
    end.                                             


forms(Checksum, Module, FuncAsts, Info) ->
    InitFuncAstBody = case Info#ast_info.global_asts of
        [] -> 
            [erl_syntax:tuple([erl_syntax:atom("error"),
                erl_syntax:atom("no_global_code")])];
        List -> 
            lists:reverse([erl_syntax:atom(ok) | lists:reverse(List)]) 
    end,
    InitFuncAst = erl_syntax:function(erl_syntax:atom("jsinit"),
        [erl_syntax:clause([], none, InitFuncAstBody)]),    
        
    ResetFuncAstFunBodyCase = erl_syntax:application(erl_syntax:atom(string), 
        erl_syntax:atom(str), [erl_syntax:variable("KeyString"), erl_syntax:string("js_")]),
    ResetFuncAstFunBody = [
        erl_syntax:match_expr(
            erl_syntax:tuple([erl_syntax:variable("Key"), erl_syntax:underscore()]), 
                erl_syntax:variable("X")),
        erl_syntax:match_expr(erl_syntax:variable("KeyString"),
            erl_syntax:application(none, erl_syntax:atom(atom_to_list), 
                [erl_syntax:variable("Key")])),
        erl_syntax:case_expr(ResetFuncAstFunBodyCase, [
            erl_syntax:clause([erl_syntax:integer(1)], none, [
                erl_syntax:application(none, erl_syntax:atom(erase),
                    [erl_syntax:variable("Key")])]),
            erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(ok)])])],           
    ResetFuncAstBody = [
        erl_syntax:application(erl_syntax:atom(lists), erl_syntax:atom(map), [
            erl_syntax:fun_expr([erl_syntax:clause(
                [erl_syntax:variable("X")], none, ResetFuncAstFunBody)]),
            erl_syntax:application(none, erl_syntax:atom(get), [])]),
        erl_syntax:atom(ok)],
    ResetFuncAst = erl_syntax:function(erl_syntax:atom("jsreset"),
       [erl_syntax:clause([], none, ResetFuncAstBody)]),
    
    ChecksumFuncAst = erl_syntax:function(erl_syntax:atom("checksum"),
          [erl_syntax:clause([], none, [erl_syntax:string(Checksum)])]),
                      
    ModuleAst = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    ExportInit = erl_syntax:arity_qualifier(erl_syntax:atom("jsinit"), erl_syntax:integer(0)),
    ExportReset = erl_syntax:arity_qualifier(erl_syntax:atom("jsreset"), erl_syntax:integer(0)),
    ExportChecksum = erl_syntax:arity_qualifier(erl_syntax:atom("checksum"), erl_syntax:integer(0)),
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list([ExportInit, ExportReset, ExportChecksum | Info#ast_info.export_asts])]),
    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, InitFuncAst, ResetFuncAst, ChecksumFuncAst | FuncAsts]].
        
 
compile_forms(Forms, Context) ->  
    Options = case Context#js_context.verbose of
        true ->
            [verbose,report_errors,report_warnings];
        _ ->
            []
    end,  
    case compile:forms(Forms, Options) of
        {ok, Module1, Bin} ->           
            Path = filename:join([Context#js_context.out_dir, atom_to_list(Module1) ++ ".beam"]),
            case file:write_file(Path, Bin) of
                ok ->
                    code:purge(Module1),
                    case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
                        {module, _} -> ok;
                        _ -> {error, "code reload failed"}
                    end;
                {error, Reason} ->
                    {error, lists:concat(["beam generation failed (", Reason, "): ", Path])}
            end;
        error ->
            {error, "compilation failed"}
    end.
    
  
body_ast(JsParseTree, Context, Scopes) when is_list(JsParseTree) ->
    {AstInfoList, {_, Scopes1}} = lists:mapfoldl(fun ast/2, {Context, Scopes}, JsParseTree),  
    {AstList, Info} = lists:mapfoldl(
        fun
            ({XAst, XInfo}, InfoAcc) ->
                {XAst, merge_info(XInfo, InfoAcc)}
        end, #ast_info{}, AstInfoList),
    {lists:flatten(AstList), Info, Scopes1};     
body_ast(JsParseTree, Context, Scopes) ->
    {{Ast, Info}, {_, Scopes1}} = ast(JsParseTree, {Context, Scopes}),
    {Ast, Info, Scopes1}.


ast({identifier, _L, true}, {Context, Scopes}) -> 
    {{erl_syntax:atom(true), #ast_info{}}, {Context, Scopes}};
ast({identifier, _L, false}, {Context, Scopes}) -> 
    {{erl_syntax:atom(false), #ast_info{}}, {Context, Scopes}};    
ast({integer, _L, Value}, {Context, Scopes}) -> 
    {{erl_syntax:integer(Value), #ast_info{}}, {Context, Scopes}};
ast({string, _L, Value}, {Context, Scopes}) -> 
    {{erl_syntax:string(Value), #ast_info{}}, {Context, Scopes}}; %% TODO: binary
ast({{'[', _L},  Value}, {Context, Scopes}) -> 
    %% TODO: implementation and tests, this just works for empty lists
    {{erl_syntax:list(Value), #ast_info{}}, {Context, Scopes}};
ast({identifier, _L, Name}, {Context, Scopes}) ->  
    var_name(Name, Context, Scopes);
ast({{identifier, _L, Name}, Value}, {Context, Scopes}) ->  
    var_init(Name, Value, Context, Scopes);             
ast({{var, _L}, DeclarationList}, {Context, Scopes}) ->
    {Ast, Info, Scopes1} = body_ast(DeclarationList, Context#js_context{action = set}, Scopes),
    {{Ast, Info}, {Context, Scopes1}};
ast({return, _L}, {Context, Scopes}) -> 
    %% TODO: eliminate this clause by adjusting the grammar
    empty_ast(Context, Scopes);
ast({{return, _L}, Expression}, {Context, Scopes}) -> 
    %% TODO: implementation and tests, this just works for return ign literals
    ast(Expression, {Context, Scopes});
ast({{function, _L1}, {identifier, _L2, Name}, {params, Params, body, Body}}, {Context, Scopes}) ->
    func(Name, Params, Body, Context, Scopes);      
ast({{{identifier, _L, Name}, MemberList}, {'(', Args}}, {Context, Scopes}) ->
    call(Name, MemberList, Args, Context, Scopes);
ast({{'=', _}, {identifier, _, Name}, Expression}, {Context, Scopes}) ->  
    {{LeftAst, _}, {_, Scopes1}} = var_name(Name, Context#js_context{action = set}, Scopes),  
    {RightAst, Info, _} = body_ast(Expression, Context, Scopes),   
    Ast = erl_syntax:match_expr(LeftAst, RightAst),  
    {{Ast, Info}, {Context, Scopes1}};   
ast({{'*' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};      
ast({{'/' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};
ast({{'<<' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};      
ast({{'>>' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};
ast({{'+' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};      
ast({{'-' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};        
ast({{'<' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};
ast({{'>' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};      
ast({{'<=' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};    
ast({{'>=' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};    
ast({{'==' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};
ast({{'!=' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};      
ast({{'===' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};    
ast({{'!==' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};     
ast({{'&' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};    
ast({{'^' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};
ast({{'|' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};      
ast({{'&&' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};    
ast({{'||' = Op, _}, L, R}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(L, Ctx, Scopes), body_ast(R, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};
ast({'cond' = Op, Cond, Expr1, Expr2}, {Ctx, Scopes}) ->      
    Ast = op(Op, body_ast(Cond, Ctx, Scopes), body_ast(Expr1, Ctx, Scopes), 
        body_ast(Expr2, Ctx, Scopes)),
    {{Ast, #ast_info{}}, {Ctx, Scopes}};     
ast(Unknown, _) ->
    throw({error, lists:concat(["Unknown token: ", Unknown])}).
    
 
empty_ast(Context, Scopes) ->
    {{[], #ast_info{}}, {Context, Scopes}}.  
    
      
var_name(Name, #js_context{action = set} = Context, Scopes) ->
    Scope = hd(Scopes),
    Name1 = erl_syntax_lib:new_variable_name(Scope#scope.names_used_set),
    Names = [Name1 | Scope#scope.names],
    Set = sets:add_element(Name1, Scope#scope.names_used_set),
    Dict = dict:store(Name, Name1, Scope#scope.names_dict),
    Scope1 = #scope{names = Names, names_used_set = Set, names_dict = Dict},
    {{erl_syntax:variable(Name1), #ast_info{}}, {Context, [Scope1 | tl(Scopes)]}}; 
var_name(Name, #js_context{action = get} = Context, Scopes) ->
    case name_search(Name, Scopes, []) of
        undefined ->
            throw({error, lists:concat(["ReferenceError: ", Name, " is not defined"])});
        {global, Name1, Scopes1} ->
            Args = [erl_syntax:atom(Name1)], 
            Ast = erl_syntax:application(none, erl_syntax:atom(get), Args),
            {{Ast, #ast_info{}}, {Context, Scopes1}};
        {Name1, Scopes1} ->
            {{erl_syntax:variable(Name1), #ast_info{}}, {Context, Scopes1}}
    end.

var_init(Name, [], Context, [GlobalScope]) ->      
    Names = lists:usort([js_name(Name) | GlobalScope#scope.names]),
    Dict = dict:store(Name, js_name(Name), GlobalScope#scope.names_dict),
    GlobalScope1 = #scope{names = Names, names_dict = Dict},
    Args = [erl_syntax:atom(js_name(Name)), erl_syntax:atom(declared)], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args), 
    {{[], #ast_info{global_asts = [Ast]}}, {Context, [GlobalScope1]}}; 
var_init(Name, Value, Context, [GlobalScope]) ->
    Names = lists:usort([js_name(Name) | GlobalScope#scope.names]),
    Dict = dict:store(Name, js_name(Name), GlobalScope#scope.names_dict),
    GlobalScope1 = #scope{names = Names, names_dict = Dict},
    {ValueAst, Info1, Scopes2} = body_ast(Value, Context, [GlobalScope1]),
    Args = [erl_syntax:atom(js_name(Name)), ValueAst], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args), 
    Asts = [Ast | Info1#ast_info.global_asts],
    {{[], Info1#ast_info{global_asts = Asts}}, {Context, Scopes2}};  
var_init(Name, [], Context, Scopes) ->
    {{AstVariable, _}, {_, Scopes1}}  = var_name(Name, Context, Scopes),
    Ast = erl_syntax:match_expr(AstVariable, erl_syntax:atom(declared)),  
    {{Ast, #ast_info{}}, {Context, Scopes1}};  
var_init(Name, Value, Context, Scopes) ->
    {{AstVariable, _}, {_, Scopes1}}  = var_name(Name, Context, Scopes),
    {AstValue, Info, Scopes2} = body_ast(Value, Context, Scopes1),
    Ast = erl_syntax:match_expr(AstVariable, AstValue),  
    {{Ast, Info}, {Context, Scopes2}}.
    
  
name_search(_, [], _) ->
    undefined;
name_search(Name, [H | T], Acc) ->
    case dict:find(Name, H#scope.names_dict) of
        {ok, Value} ->
            Scopes = lists:merge(lists:reverse(Acc), [H | T]),
            case T of
                [] ->
                    {global, Value, Scopes};
                _ ->
                    {Value, Scopes}
            end;
        error ->
            name_search(Name, T, [H | Acc]) 
    end.
             
            
func(Name, Params, Body, Context, Scopes) -> 
    {Params1, _, _} = body_ast(Params, Context, Scopes),          
    {Ast, Info, _} = body_ast(Body, Context#js_context{global = false}, push(Scopes)),
    Ast1 = erl_syntax:function(erl_syntax:atom(js_name(Name)),
        [erl_syntax:clause(Params1, none, Ast)]),
    case Context#js_context.global of
        true->
            Export = erl_syntax:arity_qualifier(erl_syntax:atom(js_name(Name)), 
                erl_syntax:integer(length(Params))),
            Exports = [Export | Info#ast_info.export_asts], 
            {{Ast1, Info#ast_info{export_asts = Exports}}, {Context, Scopes}};
        _ ->
            {{Ast1, Info}, {Context, Scopes}}
    end.


    
call(Name, MemberList, Args, Context, Scopes) ->
    {Ast, Scopes2} = case check_call(Name, MemberList) of
        {Module, Function} ->
            {Args1, _, Scopes1} = body_ast(Args, Context, Scopes),    
            {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Function), Args1),
                Scopes1};
        _ ->
            io:format("TRACE ~p:~p DOT ~p~n",[?MODULE, ?LINE, error]),
            {[], Scopes}
    end,
    case Context#js_context.global of
        true->
            {{[], #ast_info{global_asts = [Ast]}}, {Context, Scopes2}};
        _ ->
            {{Ast, #ast_info{}}, {Context, Scopes2}}
    end.
   
op('*' = Op, {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator(Op), R);
op('/' = Op, {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator(Op), R);
op('+' = Op, {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator(Op), R);
op('-' = Op, {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator(Op), R);
op('<<', {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator("bsl"), R);
op('>>', {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator("bsr"), R);    
op('<' = Op, {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator(Op), R);
op('>' = Op, {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator(Op), R);
op('<=', {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator('=<'), R);
op('>=' = Op, {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator(Op), R);
op('==' = Op, {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator(Op), R);
op('!=' , {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator('/='), R);
op('===', {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator('=:='), R);
op('!==', {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator('=/='), R);
op('&', {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator('band'), R);
op('^', {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator('bxor'), R);
op('|' , {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator('bor'), R);
op('&&', {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator('and'), R);
op('||', {L, _, _}, {R, _, _}) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(L, erl_syntax:operator('or'), R).
    
op('cond', {Cond, _, _}, {Expr1, _, _}, {Expr2, _, _}) ->
    %% TODO: dynamic typechecking    
    erl_syntax:case_expr(Cond, [
        erl_syntax:clause([erl_syntax:atom(true)], none, [Expr1]),
        erl_syntax:clause([erl_syntax:underscore()], none, [Expr2])]).
 
        
merge_info(Info1, Info2) ->
    #ast_info{
        export_asts = lists:merge(Info1#ast_info.export_asts, Info2#ast_info.export_asts),
        global_asts = lists:merge(Info1#ast_info.global_asts, Info2#ast_info.global_asts)}. 
    


js_name(Name) ->
    lists:concat(["js_", Name]).
            
push(Scopes) ->
    [#scope{} | Scopes].
    
               
check_call(console, [log])  ->
    {erlyjs_api_console, log};
check_call(_, _) ->
    error.
    
    
trace(Module, Line, Title, Content, Context) ->
    case Context#js_context.verbose of
        true ->
            io:format("TRACE ~p:~p ~p: ~p~n",[Module, Line, Title, Content]);
        _ ->
            ok
    end.