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


-record(js_ctx, {
    out_dir = "ebin",
    global = true,   
    args = [],
    api_namespace = [],
    reader = {file, read_file},
    action = get,
    force_recompile = false,
    module = [],
    verbose = false}).
    
-record(ast_inf, {
    vars = [],
    export_asts = [],
    global_asts = [],
    internal_func_asts = []}).

-record(scope, {
    names_dict = dict:new()}).
    
-record(tree_acc, {
    names_set = sets:new(),
    js_scopes = [#scope{}],
    var_pairs = [],
    func_counter = 0}).
 
 
compile(File, Module) ->
    compile(File, Module, []).
     
compile(File, Module, Options) ->
    Ctx = init_js_ctx(File, Module, Options),
    {M, F} = Ctx#js_ctx.reader,
    case catch M:F(File) of
        {ok, Data} ->
            crypto:start(),
            CheckSum = binary_to_list(crypto:sha(Data)),
            case parse(CheckSum, Data, Ctx) of    
                ok ->
                    ok;
                {ok, JsParseTree} ->
                    trace(?MODULE, ?LINE, "JsParseTree", JsParseTree, Ctx),
                    try body_ast(JsParseTree, Ctx, #tree_acc{}) of
                        {AstList, Info, _} ->                
                            Forms = forms(CheckSum, Module, AstList, Info),  
                            trace(?MODULE, ?LINE, "Forms", Forms, Ctx),                            
                            compile_forms(Forms, Ctx)                                      
                    catch 
                        throw:Error ->
                            Error
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

init_js_ctx(_File, Module, Options) ->
    Ctx = #js_ctx{},
    #js_ctx{ 
        module = list_to_atom(Module),
        out_dir = proplists:get_value(out_dir, Options,  Ctx#js_ctx.out_dir),
        verbose = proplists:get_value(verbose, Options, Ctx#js_ctx.verbose),
        reader = proplists:get_value(reader, Options, Ctx#js_ctx.reader),
        force_recompile = proplists:get_value(force_recompile, Options, Ctx#js_ctx.force_recompile)}.
      
        
parse(Data) ->
    case erlyjs_lexer:string(binary_to_list(Data)) of
        {ok, Tokens, _} ->
            erlyjs_parser:parse(Tokens);
        Err ->
            Err
    end.
        

parse(_, Data, #js_ctx{force_recompile = true}) ->  
    parse(Data);
parse(CheckSum, Data, Ctx) ->
    Module = Ctx#js_ctx.module,
    case catch Module:checksum() of
        CheckSum ->   
            ok;
        _ ->
            parse(Data)
    end.                                             


forms(Checksum, Module, FuncAsts, Info) ->
    FuncAsts2 = lists:append([FuncAsts, Info#ast_inf.internal_func_asts]),
    
    InitFuncAstBody = case Info#ast_inf.global_asts of
        [] -> 
            [erl_syntax:tuple([erl_syntax:atom("error"),
                erl_syntax:atom("no_global_code")])];
        List -> 
            lists:reverse([erl_syntax:atom(ok) | List]) 
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
        [erl_syntax:list([ExportInit, ExportReset, ExportChecksum | Info#ast_inf.export_asts])]),
    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, InitFuncAst, ResetFuncAst, ChecksumFuncAst | FuncAsts2]].
        

compile_forms(Forms, Ctx) ->  
    CompileOptions = case Ctx#js_ctx.verbose of
        true -> [verbose, report_errors, report_warnings];
        _ -> []
    end,  
    case compile:forms(Forms, CompileOptions) of
        {ok, Module1, Bin} -> 
            case Ctx#js_ctx.verbose of
                true ->
                    io:format("Erlang source:~n~n"),
                    io:put_chars(erl_prettypr:format(erl_syntax:form_list(Forms))),
                    io:format("~n");
                _ -> ok
            end,        
            Path = filename:join([Ctx#js_ctx.out_dir, atom_to_list(Module1) ++ ".beam"]),
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
    
  
body_ast(JsParseTree, Ctx, Acc) when is_list(JsParseTree) ->
    {AstInfList, {_, Acc1}} = lists:mapfoldl(fun ast/2, {Ctx, Acc}, JsParseTree),  
    {AstList, Inf} = lists:mapfoldl(
        fun
            ({XAst, XInf}, InfAcc) ->
                {XAst, append_info(XInf, InfAcc)}
        end, #ast_inf{}, AstInfList),
    {lists:flatten(AstList), Inf, Acc1};
body_ast(JsParseTree, Ctx, Acc) ->
    {{Ast, Inf}, {_, Acc1}} = ast(JsParseTree, {Ctx, Acc}),
    {Ast, Inf, Acc1}.


ast({identifier, _, true}, {Ctx, Acc}) -> 
    {{erl_syntax:atom(true), #ast_inf{}}, {Ctx, Acc}};
ast({identifier, _, false}, {Ctx, Acc}) -> 
    {{erl_syntax:atom(false), #ast_inf{}}, {Ctx, Acc}};    
ast({integer, _, Value}, {Ctx, Acc}) -> 
    {{erl_syntax:integer(Value), #ast_inf{}}, {Ctx, Acc}};
ast({float, _, Value}, {Ctx, Acc}) -> 
    {{erl_syntax:float(Value), #ast_inf{}}, {Ctx, Acc}};
ast({string, _, Value}, {Ctx, Acc}) ->
    {{erl_syntax:string(Value), #ast_inf{}}, {Ctx, Acc}}; %% TODO: binary instead of string 
ast({{'[', _L},  Value}, {Ctx, Acc}) -> 
    %% TODO: implementation and tests, this just works for empty lists
    {{erl_syntax:list(Value), #ast_inf{}}, {Ctx, Acc}};
ast({{identifier, _, undefined}, _}, {Ctx, Acc}) ->  
    {{erl_syntax:atom(undefined), #ast_inf{}}, {Ctx, Acc}};    
ast({{identifier, _, 'Infinity'}, _}, {Ctx, Acc}) ->  
    {{erl_syntax:atom('Infinity'), #ast_inf{}}, {Ctx, Acc}};
ast({{identifier, _, 'NaN'}, _}, {Ctx, Acc}) ->  
    {{erl_syntax:atom('NaN'), #ast_inf{}}, {Ctx, Acc}};
ast({identifier, _, Name}, {Ctx, Acc}) ->  
    var_ast(Name, Ctx, Acc);
ast({{identifier, _, Name} , {'(', Args}}, {Ctx, Acc}) ->
    call(Name, Args, Ctx, Acc);  
ast({{{identifier, _, Name}, Names}, {'(', Args}}, {Ctx, Acc}) ->
    call(Name, Names, Args, Ctx, Acc);  
ast({{identifier, _, Name}, Value}, {Ctx, Acc}) -> 
    var_declare(Name, Value, Ctx, Acc);  
ast({var, DeclarationList}, {Ctx, Acc}) ->
    {Ast, Info, Acc1} = body_ast(DeclarationList, Ctx#js_ctx{action = set}, Acc),
    {{Ast, Info}, {Ctx, Acc1}};
ast(return, {Ctx, Acc}) -> 
    %% TODO: eliminate this clause by adjusting the grammar
    empty_ast(Ctx, Acc);
ast({return, Expression}, {Ctx, Acc}) -> 
    %% TODO: implementation and tests, this doesnt't work generally
    ast(Expression, {Ctx, Acc});
ast({function, {identifier, _L2, Name}, {params, Params, body, Body}}, {Ctx, Acc}) ->
    func(Name, Params, Body, Ctx, Acc);   
ast({op, {Op, _}, In}, {Ctx, Acc}) ->
    {Out, _, #tree_acc{names_set = Set}} = body_ast(In, Ctx, Acc),
    {{erlyjs_operators:ast(Op, Out), #ast_inf{}}, {Ctx, Acc#tree_acc{names_set = Set}}};
ast({op, {Op, _}, In1, In2}, {Ctx, Acc}) ->
    {Out1, _, #tree_acc{names_set = Set1}} = body_ast(In1, Ctx, Acc),
    {Out2, _, #tree_acc{names_set = Set2}} = body_ast(In2, Ctx, Acc#tree_acc{names_set = Set1}),
    {{erlyjs_operators:ast(Op, Out1, Out2), #ast_inf{}}, {Ctx, Acc#tree_acc{names_set = Set2}}};
ast({op, {{Op, postfix}, _}, In1, In2}, {Ctx, Acc}) ->
    %% TODO: fix this, code below is prefix operator
    {Out1, _, #tree_acc{names_set = Set1}} = body_ast(In1, Ctx, Acc),
    {Out2, _, #tree_acc{names_set = Set2}} = body_ast(In2, Ctx, Acc#tree_acc{names_set = Set1}),
    {{erlyjs_operators:ast(Op, Out1, Out2), #ast_inf{}}, {Ctx, Acc#tree_acc{names_set = Set2}}};
ast({op, Op, In1, In2, In3}, {Ctx, Acc}) ->
    {Out1, _, #tree_acc{names_set = Set1}} = body_ast(In1, Ctx, Acc),
    {Out2, _, #tree_acc{names_set = Set2}} = body_ast(In2, Ctx, Acc#tree_acc{names_set = Set1}),
    {Out3, _, #tree_acc{names_set = Set3}} = body_ast(In3, Ctx, Acc#tree_acc{names_set = Set2}),
    {{erlyjs_operators:ast(Op, Out1, Out2, Out3), #ast_inf{}}, {Ctx, Acc#tree_acc{names_set = Set3}}}; 
ast({assign, {'=', _}, {identifier, _, Name}, In1}, {Ctx, Acc}) ->  
    {{Out2, _}, {_, Acc1}} = var_ast(Name, Ctx#js_ctx{action = set}, Acc),  
    {Out3, Inf, _} = body_ast(In1, Ctx, Acc),  
    assign_ast('=', Name, Out2, Out3, Inf, Ctx, Acc1);
ast({assign, {Op, _}, {identifier, _, Name}, In1}, {Ctx, Acc}) ->  
    {{Out2, _}, _} = var_ast(Name, Ctx, Acc),  
    {Out3, Inf, Acc1} = body_ast(In1, Ctx, Acc),    
    {{Out4, _}, {_, Acc2}} = var_ast(Name, Ctx#js_ctx{action = set}, Acc1), 
    assign_ast('=', Name, Out4, erlyjs_operators:ast(assign_to_op(Op), Out2, Out3), Inf, Ctx, Acc2);  
ast({'if', Cond, If}, {#js_ctx{global = true} = Ctx, Acc}) -> 
    {OutCond, _, #tree_acc{names_set = Set}} = body_ast(Cond, Ctx, Acc),
    Acc2 = Acc#tree_acc{names_set = Set, var_pairs = push_var_pairs(Acc)},
    {_, Inf, Acc3} = body_ast(If, Ctx, Acc2),
    ReturnVarsElse = get_vars_init(Acc, Acc3, Ctx),                                        
    Ast = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none, Inf#ast_inf.global_asts),
        erl_syntax:clause([erl_syntax:underscore()], none, [ReturnVarsElse])]),
    {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, pop_var_pairs(Acc3)}};    
ast({'if', Cond, If}, {Ctx, Acc}) -> 
    {OutCond, _, #tree_acc{names_set = Set}} = body_ast(Cond, Ctx, Acc),
    Acc2 = Acc#tree_acc{names_set = Set, var_pairs = push_var_pairs(Acc)},
    {OutIf, _, Acc3} = body_ast(If, Ctx, Acc2),
    ReturnVarsIf = get_vars_snapshot(Acc3),    
    ReturnVarsElse = get_vars_init(Acc, Acc3, Ctx),
    {Vars, Acc4} =  get_vars_result(Acc3, Acc3, Ctx),                                         
    Ast = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none, append_asts(OutIf, ReturnVarsIf)),
        erl_syntax:clause([erl_syntax:underscore()], none, [ReturnVarsElse])]),
    Ast2 = erl_syntax:match_expr(Vars, Ast),
    {{Ast2, #ast_inf{}}, {Ctx, pop_var_pairs(Acc4)}};
ast({'if', Cond, If, Else}, {Ctx, Acc}) -> 
    %% TODO: refactor and handle global scope
    {OutCond, _, #tree_acc{names_set = Set}} = body_ast(Cond, Ctx, Acc),
    AccOutIfIn = Acc#tree_acc{names_set = Set, var_pairs = push_var_pairs(Acc)},
    {OutIf, _, #tree_acc{names_set = Set1} = Acc1} = body_ast(If, Ctx, AccOutIfIn), 
    AccOutElseIn = Acc#tree_acc{names_set = Set1, var_pairs = [dict:new() | Acc#tree_acc.var_pairs]},
    {OutElse, _, Acc2} = body_ast(Else, Ctx, AccOutElseIn),
    KeyList = lists:usort([ Key || {Key, _} <- lists:append([
        dict:to_list(hd(Acc1#tree_acc.var_pairs)), 
        dict:to_list(hd(Acc2#tree_acc.var_pairs))])]),
    %% ReturnVarsIf = get_vars(Acc, Acc1, Ctx),
    ReturnVarsIf = erl_syntax:tuple(lists:map(
        fun
            (Key) ->
                case  dict:find(Key, hd(Acc1#tree_acc.var_pairs)) of
                    {ok, Val} ->
                       erl_syntax:variable(Val);
                    error ->
                       {{Ast, _}, {_, _}} = var_ast(Key, Ctx, Acc),
                       Ast
                end
        end, KeyList)), 
    %% ReturnVarsElse = get_vars(Acc, Acc2, Ctx),         
    ReturnVarsElse = erl_syntax:tuple(lists:map(
        fun
            (Key) ->
                case  dict:find(Key, hd(Acc2#tree_acc.var_pairs)) of
                    {ok, Val} ->
                       erl_syntax:variable(Val);
                    error ->
                       {{Ast, _}, {_, _}} = var_ast(Key, Ctx, Acc),
                       Ast
                end
        end, KeyList)),    
    %% {Vars, Acc3} = get_vars_result(Acc2, Acc2, Ctx),    
    {Vars, Acc3} = lists:mapfoldl(
        fun
            (Key, AccIn) ->
                
                {{Ast, _}, {_, AccOut}} = var_ast(Key, Ctx#js_ctx{action = set}, AccIn),
                {Ast, AccOut}
        end,  Acc2, KeyList),         
    Ast = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none, append_asts(OutIf, ReturnVarsIf)),
        erl_syntax:clause([erl_syntax:underscore()], none, append_asts(OutElse, ReturnVarsElse))]),
    Ast2 = erl_syntax:match_expr(erl_syntax:tuple(Vars), Ast),
    {{Ast2, #ast_inf{}}, {Ctx, pop_var_pairs(Acc3)}};    
ast({do_while, Stmt, Cond}, {#js_ctx{global = true} = Ctx, Acc}) ->   
    Acc2 = Acc#tree_acc{
        var_pairs = push_var_pairs(Acc),
        func_counter = inc_func_counter(Acc)},   
    {_, Inf, Acc3} = body_ast(Stmt, Ctx, Acc2),   
    {OutCond, _, Acc4} = body_ast(Cond, Ctx, Acc3),
    AstFuncCond = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none,
            [erl_syntax:application(none, func_name(Acc2), [])]),
        erl_syntax:clause([erl_syntax:underscore()], none,  
            [get_global_vars(Acc3)])]),   
    Func = erl_syntax:function(func_name(Acc2),
        [erl_syntax:clause([], none, append_asts(Inf#ast_inf.global_asts, AstFuncCond))]),
    Ast = erl_syntax:application(none, func_name(Acc2), []),
    {{[], #ast_inf{internal_func_asts = [Func], global_asts = [Ast]}}, {Ctx, pop_var_pairs(Acc4)}};
ast({do_while, Stmt, Cond}, {Ctx, Acc}) ->  
    Acc2 = Acc#tree_acc{
        var_pairs = push_var_pairs(Acc),
        func_counter = inc_func_counter(Acc)},   
    {OutStmt, _, Acc3} = body_ast(Stmt, Ctx, Acc2),   
    {OutCond, _, Acc4} = body_ast(Cond, Ctx, Acc3),
    VarsBefore = get_vars_init(Acc, Acc3, Ctx),
    VarsAfterStmt = get_vars_snapshot(Acc3),
    {VarsAfter, Acc5} = get_vars_result(Acc3, Acc4, Ctx),    
    AstFuncCond = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none,
            [erl_syntax:application(none, func_name(Acc2), [VarsAfterStmt])]),
        erl_syntax:clause([erl_syntax:underscore()], none,  
            [VarsAfterStmt])]),
    Func = erl_syntax:function(func_name(Acc2),
        [erl_syntax:clause([VarsBefore], none, append_asts(OutStmt, AstFuncCond))]),
    Ast = erl_syntax:match_expr(VarsAfter, 
        erl_syntax:application(none, func_name(Acc2), [VarsBefore])),  
    {{[Ast], #ast_inf{internal_func_asts = [Func]}}, {Ctx, pop_var_pairs(Acc5)}};       
ast({while, Cond, Stmt}, {#js_ctx{global = true} = Ctx, Acc}) ->
    {OutCond, _, #tree_acc{names_set = Set}} = body_ast(Cond, Ctx, Acc),
    Acc2 = Acc#tree_acc{
        names_set = Set, 
        var_pairs = push_var_pairs(Acc),
        func_counter = inc_func_counter(Acc)},
    {_, Inf, Acc3} = body_ast(Stmt, Ctx, Acc2),
    AstFuncCond = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none,
            append_asts(Inf#ast_inf.global_asts, erl_syntax:application(none, func_name(Acc3), []))),
        erl_syntax:clause([erl_syntax:underscore()], none,  
            [get_global_vars(Acc3)])]),    
    Func = erl_syntax:function(func_name(Acc3),
        [erl_syntax:clause([], none, [AstFuncCond])]),
    Ast = erl_syntax:application(none, func_name(Acc3), []),        
    {{[], #ast_inf{internal_func_asts = [Func], global_asts = [Ast]}}, {Ctx, pop_var_pairs(Acc3)}};      
ast({while, Cond, Stmt}, {Ctx, Acc}) ->
    {OutCond, _, #tree_acc{names_set = Set}} = body_ast(Cond, Ctx, Acc),
    Acc2 = Acc#tree_acc{
        names_set = Set, 
        var_pairs = push_var_pairs(Acc),
        func_counter = inc_func_counter(Acc)},
    {OutStmt, _, Acc3} = body_ast(Stmt, Ctx, Acc2),
    VarsBefore = get_vars_init(Acc, Acc3, Ctx),
    VarsAfterStmt = get_vars_snapshot(Acc3),
    {VarsAfter, Acc4} = get_vars_result(Acc3, Acc3, Ctx),    
    AstFuncCond = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none,
            append_asts(OutStmt, erl_syntax:application(none, func_name(Acc2), [VarsAfterStmt]))),
        erl_syntax:clause([erl_syntax:underscore()], none,  
            [VarsBefore])]),          
    Func = erl_syntax:function(func_name(Acc2),
        [erl_syntax:clause([VarsBefore], none, [AstFuncCond])]),
    Ast = erl_syntax:match_expr(VarsAfter, 
        erl_syntax:application(none, func_name(Acc3), [VarsBefore])),           
    {{[Ast], #ast_inf{internal_func_asts = [Func]}}, {Ctx, pop_var_pairs(Acc4)}}; 
ast({for, Init, Cond, Final, Stmt}, {Ctx, Acc}) -> 
    %% TODO: refactor and handle global scope
    {OutInit, _, Acc2} = body_ast(Init, Ctx, Acc),
    Acc3 = Acc2#tree_acc{
        var_pairs = push_var_pairs(Acc),
        func_counter = inc_func_counter(Acc)},
    {OutCond, _, Acc4} = body_ast(Cond, Ctx, Acc3),
    {OutStmt, _, Acc5} = body_ast(Stmt, Ctx, Acc4),                
    {FinalExpr, _, Acc6} = body_ast(Final, Ctx, Acc5),
    VarsBefore = get_vars_init(Acc2, Acc6, Ctx),
    VarsAfterStmt = get_vars_snapshot(Acc6),
    {VarsAfter, Acc7} = get_vars_result(Acc6, Acc6, Ctx),
    Stmts = append_asts(OutStmt, FinalExpr), %%% only works with full assignment expression as FinalExpr
    AstFuncCond = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none,
            append_asts(Stmts, erl_syntax:application(none, func_name(Acc5), [VarsAfterStmt]))),
        erl_syntax:clause([erl_syntax:underscore()], none,  
            [VarsBefore])]),
    Func = erl_syntax:function(func_name(Acc5),
        [erl_syntax:clause([VarsBefore], none, [AstFuncCond])]),
    Ast = erl_syntax:match_expr(VarsAfter, 
        erl_syntax:application(none, func_name(Acc6), [VarsBefore])),    
    {{[OutInit, Ast], #ast_inf{internal_func_asts = [Func]}}, {Ctx, pop_var_pairs(Acc7)}}; 
ast(Unknown, _) ->
    throw({error, lists:concat(["Unknown token: ", Unknown])}). 
    
    
empty_ast(Ctx, Acc) ->
    {{[], #ast_inf{}}, {Ctx, Acc}}.  
 
 
func_name(Acc) ->
    erl_syntax:atom(lists:concat(["func_", Acc#tree_acc.func_counter])).
    
    
%%  TODO:
%% 
%% fun_var_ast(Acc) ->
%%     ErlName = erl_syntax_lib:new_variable_name(Acc#tree_acc.names_set),
%%     Acc2 = Acc#tree_acc{names_set = sets:add_element(ErlName, Acc#tree_acc.names_set)},
%%     {erl_syntax:variable(ErlName), Acc2}.
    
      
var_ast(Key, #js_ctx{action = set} = Ctx, Acc) ->
    Scope = hd(Acc#tree_acc.js_scopes),
    ErlName = erl_syntax_lib:new_variable_name(Acc#tree_acc.names_set), 
    Dict = dict:store(Key, ErlName, Scope#scope.names_dict),
    VarPairs = case Acc#tree_acc.var_pairs of
        [] ->
            [];
        Val ->
            [dict:store(Key, ErlName, hd(Val)) | tl(Val)]
    end,
    Acc1 = Acc#tree_acc{names_set = sets:add_element(ErlName, Acc#tree_acc.names_set), 
        js_scopes = [#scope{names_dict = Dict} | tl(Acc#tree_acc.js_scopes)],
        var_pairs = VarPairs},
    {{erl_syntax:variable(ErlName), #ast_inf{}}, {Ctx, Acc1}}; 
    
var_ast(undefined, #js_ctx{action = get} = Ctx, Acc) ->
    {{erl_syntax:atom(undefined), #ast_inf{}}, {Ctx, Acc}};
var_ast('Infinity', #js_ctx{action = get} = Ctx, Acc) ->
    {{erl_syntax:atom('Infinity'), #ast_inf{}}, {Ctx, Acc}};
var_ast('NaN', #js_ctx{action = get} = Ctx, Acc) ->
    {{erl_syntax:atom('NaN'), #ast_inf{}}, {Ctx, Acc}};        
var_ast(Key, #js_ctx{action = get} = Ctx, Acc) ->
    case name_search(Key, Acc#tree_acc.js_scopes, []) of
        not_found ->
            throw({error, lists:concat(["ReferenceError: ", Key, " is not defined"])});
        {global, Name} ->
            Args = [erl_syntax:atom(Name)], 
            Ast = erl_syntax:application(none, erl_syntax:atom(get), Args),
            {{Ast, #ast_inf{}}, {Ctx, Acc}};
        Name ->
            {{erl_syntax:variable(Name), #ast_inf{}}, {Ctx, Acc}}
    end.

var_declare(Key, [], Ctx, #tree_acc{js_scopes = [GlobalScope]}=Acc) ->      
    Dict = dict:store(Key, prefix_name(Key), GlobalScope#scope.names_dict),
    Args = [erl_syntax:atom(prefix_name(Key)), erl_syntax:atom(undefined)], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args),
    Acc2 = Acc#tree_acc{js_scopes=[#scope{names_dict = Dict}]}, 
    {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, Acc2}};    
var_declare(Key, {identifier, _, undefined}, Ctx,  #tree_acc{js_scopes = [_]}=Acc) -> 
    {_, {_, Acc2}}  = var_ast(Key, Ctx, Acc),
    Args = [erl_syntax:atom(prefix_name(Key)), erl_syntax:atom(undefined)], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args),
    {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, Acc2}}; 
var_declare(Key, {identifier, _, 'Infinity'}, Ctx,  #tree_acc{js_scopes = [_]}=Acc) -> 
    {_, {_, Acc2}}  = var_ast(Key, Ctx, Acc),
    Args = [erl_syntax:atom(prefix_name(Key)), erl_syntax:atom('Infinity')], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args),
    {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, Acc2}};    
var_declare(Key, {identifier, _, 'NaN'}, Ctx,  #tree_acc{js_scopes = [_]}=Acc) -> 
    {_, {_, Acc2}}  = var_ast(Key, Ctx, Acc),
    Args = [erl_syntax:atom(prefix_name(Key)), erl_syntax:atom('NaN')], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args),
    {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, Acc2}};           
var_declare(Key, Value, Ctx,  #tree_acc{js_scopes = [GlobalScope]}=Acc) ->      
    Dict = dict:store(Key, prefix_name(Key), GlobalScope#scope.names_dict),
    Acc2 = Acc#tree_acc{js_scopes=[#scope{names_dict = Dict}]},
    {ValueAst, Inf, Acc2} = body_ast(Value, Ctx, Acc2),
    Args = [erl_syntax:atom(prefix_name(Key)), ValueAst], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args), 
    Asts = [Ast | Inf#ast_inf.global_asts],
    {{[], Inf#ast_inf{global_asts = Asts}}, {Ctx, Acc2}};  
var_declare(Key, [], Ctx, Acc) ->
    {{AstVariable, _}, {_, Acc2}}  = var_ast(Key, Ctx, Acc),
    Ast = erl_syntax:match_expr(AstVariable, erl_syntax:atom(undefined)),  
    {{Ast, #ast_inf{}}, {Ctx, Acc2}};  
var_declare(Key, {identifier, _, undefined}, Ctx, Acc) ->
    {{AstVar, Inf}, {_, Acc2}}  = var_ast(Key, Ctx, Acc), 
    {{erl_syntax:match_expr(AstVar, erl_syntax:atom(undefined)), Inf}, {Ctx, Acc2}};
var_declare(Key, {identifier, _, 'Infinity'}, Ctx, Acc) ->
    {{AstVar, Inf}, {_, Acc2}}  = var_ast(Key, Ctx, Acc), 
    {{erl_syntax:match_expr(AstVar, erl_syntax:atom('Infinity')), Inf}, {Ctx, Acc2}};
var_declare(Key, {identifier, _, 'NaN'}, Ctx, Acc) ->
    {{AstVar, Inf}, {_, Acc2}}  = var_ast(Key, Ctx, Acc), 
    {{erl_syntax:match_expr(AstVar, erl_syntax:atom('NaN')), Inf}, {Ctx, Acc2}};                        
var_declare(Key, Value, Ctx, Acc) ->
    {{AstVariable, _}, {_, Acc2}}  = var_ast(Key, Ctx, Acc),
    {AstValue, Info, Acc3} = body_ast(Value, Ctx, Acc2),
    Ast = erl_syntax:match_expr(AstVariable, AstValue),  
    {{Ast, Info}, {Ctx, Acc3}}.
    
  
name_search(_, [], _) ->
    not_found;
name_search(Key, [H | T], Acc) ->
    case dict:find(Key, H#scope.names_dict) of
        {ok, Value} ->
            case T of
                [] -> 
                    {global, prefix_name(Key)};
                _ -> Value
            end;
        error -> name_search(Key, T, [H | Acc]) 
    end.


get_vars_init(Acc1, Acc2, Ctx) ->
    erl_syntax:tuple(dict:fold(
        fun
            (Key, _, AccIn) -> 
                {{Ast, _}, {_, _}} = var_ast(Key, Ctx, Acc1),
                [Ast| AccIn]
        end, [], hd(Acc2#tree_acc.var_pairs))).
            

get_vars_snapshot(Acc) ->                    
    erl_syntax:tuple(dict:fold(
        fun
            (_, Val, AccIn) -> 
                [erl_syntax:variable(Val) | AccIn]
        end, [], hd(Acc#tree_acc.var_pairs))).
        

get_global_vars(Acc) ->
    L = dict:to_list(hd(Acc#tree_acc.var_pairs)),
    erl_syntax:tuple([erl_syntax:application(none, erl_syntax:atom(get), 
        [erl_syntax:atom(Key)]) || {Key, _} <-  L]).



%% TODO: 
%%         
%% get_vars(AccInit, AccSnapshot, Ctx) -> 
%%     KeyList = lists:usort([ Key || {Key, _} <- lists:append([
%%         dict:to_list(hd(AccInit#tree_acc.var_pairs)), 
%%         dict:to_list(hd(AccSnapshot#tree_acc.var_pairs))])]),       
%%     erl_syntax:tuple(lists:map(
%%         fun
%%             (Key) ->
%%                 case  dict:find(Key, hd(AccSnapshot#tree_acc.var_pairs)) of
%%                     {ok, Val} ->
%%                        erl_syntax:variable(Val);
%%                     error ->
%%                        {{Ast, _}, {_, _}} = var_ast(Key, Ctx, AccInit),
%%                        Ast
%%                 end
%%         end, KeyList)).


get_vars_result(Acc, AccSet, Ctx) ->
    AccInit = Acc#tree_acc{names_set = AccSet#tree_acc.names_set},        
    {VarsAfter, Acc2} = lists:mapfoldl(
        fun
            ({Key, _}, AccIn) ->
                {{Ast, _}, {_, AccOut}} = var_ast(Key, Ctx#js_ctx{action = set}, AccIn),
                {Ast, AccOut}
            end,  AccInit, dict:to_list(hd(Acc#tree_acc.var_pairs))),  
    {erl_syntax:tuple(VarsAfter), Acc2}.
            
                       
func(Name, Params, Body, Ctx, Acc) -> 
    {Params1, _, _} = body_ast(Params, Ctx, Acc),          
    {Ast, Inf, _} = body_ast(Body, Ctx#js_ctx{global = false}, push_scope(Acc)),
    Ast1 = erl_syntax:function(erl_syntax:atom(prefix_name(Name)),
        [erl_syntax:clause(Params1, none, Ast)]),
    case Ctx#js_ctx.global of
        true->
            Export = erl_syntax:arity_qualifier(erl_syntax:atom(prefix_name(Name)), 
                erl_syntax:integer(length(Params))),
            Exports = [Export | Inf#ast_inf.export_asts], 
            {{Ast1, Inf#ast_inf{export_asts = Exports}}, {Ctx, Acc}};
        _ ->
            {{Ast1, Inf}, {Ctx, Acc}}
    end.
    
      
call(Name, Args, Ctx, Acc) ->
    Arity = length(Args),
    case native_global_func(Name, Arity) of
        ok ->     
            {Args2, _, Acc2} = body_ast(Args, Ctx, Acc),  
            Ast = erl_syntax:application(erl_syntax:atom(erlyjs_global_funcs), 
                erl_syntax:atom(Name), Args2),
            {{Ast, #ast_inf{}}, {Ctx, Acc2}};
        _ ->
            throw({error, lists:concat(["No such global function: ", 
                Name, " (arity: ", Arity, ")"])})
    end.


call(Name, Names, Args, Ctx, Acc) ->
    Arity = length(Args),
    case api_func(Name, Names, Arity) of
        {Module, Function} ->
            {Args1, _, Acc1} = body_ast(Args, Ctx, Acc),    
            Ast = erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Function), Args1),
            maybe_global({{Ast, #ast_inf{}}, {Ctx, Acc1}});
        _ ->
            throw({error, lists:concat(["No such function: ", todo_prettyprint_functionname])})
    end.
        

native_global_func(decodeURI, 1) -> ok;
native_global_func(decodeURIComponent, 1) -> ok;
native_global_func(encodeURI, 1) -> ok;
native_global_func(encodeURIComponent, 1) -> ok;
native_global_func(eval, 1) -> ok;
native_global_func(isFinite, 1) -> ok;
native_global_func(isNaN, 1) -> ok;
native_global_func(parseInt, 1) -> ok;
native_global_func(parseInt, 2) -> ok;
native_global_func(parseFloat, 1) -> ok;
native_global_func(_, _) -> false.

api_func(console = Name, [log = Func], 1)  -> build_api_func(Name, Func);
api_func(_, _, _) -> false.    

build_api_func(Name, Func) -> {lists:concat(["erlyjs_api_", Name]), Func}.
   
 
assign_ast('=', Name, _, Ast2, Inf, #js_ctx{global = true} = Ctx, Acc) ->
    %% TODO: dynamic typechecking
    Ast = erl_syntax:application(none, erl_syntax:atom(put), [
        erl_syntax:atom(prefix_name(Name)), Ast2]),
    GlobalAsts = append_asts(Inf#ast_inf.global_asts, Ast),
    {{[], #ast_inf{global_asts = GlobalAsts}}, {Ctx, Acc}};  
assign_ast('=', _, Ast1, Ast2, _, Ctx, Acc) ->
    %% TODO: dynamic typechecking  
    Ast = erl_syntax:match_expr(Ast1, Ast2),
    {{Ast, #ast_inf{}}, {Ctx, Acc}}; 
assign_ast(Unknown, _, _, _, _, _, _) ->
    throw({error, lists:concat(["Unknown assignment operator: ", Unknown])}).        


maybe_global({{Ast, Inf}, {#js_ctx{global = true} = Ctx, Acc}}) ->
    GlobalAsts = append_asts(Inf#ast_inf.global_asts, Ast),
    {{[], Inf#ast_inf{global_asts = GlobalAsts}}, {Ctx, Acc}};
maybe_global({AstInf, CtxAcc}) ->    
    {AstInf, CtxAcc}.
    
 
append_asts(Ast1, Ast2) when is_list(Ast1), is_list(Ast2) ->
    lists:append(Ast1, Ast2);
append_asts(Ast1, Ast2) when is_list(Ast1) ->
    lists:append(Ast1, [Ast2]);
append_asts(Ast1, Ast2) when is_list(Ast2) ->
    lists:append([Ast1], Ast2);
append_asts(Ast1, Ast2) ->
    [Ast1, Ast2].
    
                           
append_info(Info1, Info2) ->
    #ast_inf{
        export_asts = lists:append(Info1#ast_inf.export_asts, Info2#ast_inf.export_asts),
        global_asts = lists:append(Info1#ast_inf.global_asts, Info2#ast_inf.global_asts),
        internal_func_asts = lists:append(
            Info1#ast_inf.internal_func_asts, Info2#ast_inf.internal_func_asts)}. 


assign_to_op(Assign) ->
    list_to_atom(tl(lists:reverse(string:strip(atom_to_list(Assign), both, $')))).
    

prefix_name(Name) ->
    lists:concat(["js_", Name]).
            
push_scope(Acc) ->
    Acc#tree_acc{js_scopes = [#scope{} | Acc#tree_acc.js_scopes]}.
    

pop_var_pairs(Acc) ->  
    Acc#tree_acc{var_pairs = tl(Acc#tree_acc.var_pairs)}.


push_var_pairs(Acc) -> 
    [dict:new() | Acc#tree_acc.var_pairs].
    
    
inc_func_counter(Acc) ->    
    Acc#tree_acc.func_counter + 1.

                   
trace(Module, Line, Title, Content, Ctx) ->
    case Ctx#js_ctx.verbose of
        true ->
            io:format("TRACE ~p:~p ~p: ~p~n",[Module, Line, Title, Content]);
        _ ->
            ok
    end.