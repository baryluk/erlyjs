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
-export([parse/1, parse_transform/1, compile/2, compile/3]).


-record(js_ctx, {
    out_dir = "ebin",
    global = false,  
    args = [],
    api_namespace = [],
    reader = {file, read_file},
    action = get,
    force_recompile = false,
    module = [],
    verbose = false}).
    
-record(ast_inf, {              %%  additional info about a parse transfomed AST
    export_asts = [],           %%  Module header export statements
    global_asts = [],           %%  Exported Erlang functions
    internal_func_asts = []}).  %%  Internal Erlang functions  

-record(scope, {                %%  represents a Javascript variable scope
    names_dict = dict:new()}).  %%  Key: JsName, Value: ErlName
    
-record(trav, {                 %%  traverses the whole tree
    names_set = sets:new(),     %%  for unique Erlang variable names 
    js_scopes = [#scope{}],     %%  for Javascript variables scopes
    names = [],                 %%  for temporary use: [{JsNameAsKey, ErlName}, ...]
    func_counter = 0}).         %%  for unique internal Erlang function names
 
 
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
                    try p_t_root(JsParseTree, Ctx, #trav{}) of
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
  		
	
parse(Data) when is_binary(Data) ->
    parse(binary_to_list(Data));
parse(Data) ->
    case erlyjs_lexer:string(Data) of
        {ok, Tokens, _} ->
            erlyjs_parser:parse(Tokens);
        Err ->
            Err
    end.

    
parse_transform(JsParseTree) ->
    parse_transform(JsParseTree, #js_ctx{}, #trav{}).
    
	
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
    
 
p_t_root(JsParseTree, Ctx, Trav) -> 
    parse_transform(JsParseTree, Ctx#js_ctx{global = true}, Trav).
        
        
p_t(JsParseTree, Ctx, Trav) -> 
    parse_transform(JsParseTree, Ctx#js_ctx{global = false}, Trav).
    
  
parse_transform(JsParseTree, Ctx, Trav) when is_list(JsParseTree) ->
    {AstInfList, {_, Trav1}} = lists:mapfoldl(fun ast/2, {Ctx, Trav}, JsParseTree),   
    {AstList, Inf} = lists:mapfoldl(
        fun
            ({XAst, XInf}, InfAcc) ->
                {XAst, append_info(XInf, InfAcc)}
        end, #ast_inf{}, AstInfList),         
    {lists:flatten(AstList), Inf, Trav1};  
parse_transform(JsParseTree, Ctx, Trav) ->
    {{Ast, Inf}, {_, Trav1}} = ast(JsParseTree, {Ctx, Trav}),
    {Ast, Inf, Trav1}.


ast({identifier, _, true}, {Ctx, Trav}) -> 
    {{erl_syntax:atom(true), #ast_inf{}}, {Ctx, Trav}};
ast({identifier, _, false}, {Ctx, Trav}) -> 
    {{erl_syntax:atom(false), #ast_inf{}}, {Ctx, Trav}};    
ast({integer, _, Value}, {Ctx, Trav}) -> 
    {{erl_syntax:integer(Value), #ast_inf{}}, {Ctx, Trav}};
ast({float, _, Value}, {Ctx, Trav}) -> 
    {{erl_syntax:float(Value), #ast_inf{}}, {Ctx, Trav}};
ast({string, _, Value}, {Ctx, Trav}) ->
    {{erl_syntax:string(Value), #ast_inf{}}, {Ctx, Trav}}; %% TODO: binary instead of string 
ast({{'[', _L},  Value}, {Ctx, Trav}) -> 
    %% TODO: implementation and tests, this just works for empty lists
    {{erl_syntax:list(Value), #ast_inf{}}, {Ctx, Trav}};
ast({{identifier, _, undefined}, _}, {Ctx, Trav}) ->  
    {{erl_syntax:atom(undefined), #ast_inf{}}, {Ctx, Trav}};    
ast({{identifier, _, 'Infinity'}, _}, {Ctx, Trav}) ->  
    {{erl_syntax:atom('Infinity'), #ast_inf{}}, {Ctx, Trav}};
ast({{identifier, _, 'NaN'}, _}, {Ctx, Trav}) ->  
    {{erl_syntax:atom('NaN'), #ast_inf{}}, {Ctx, Trav}};
ast({identifier, _, Name}, {Ctx, Trav}) ->  
    var_ast(Name, Ctx, Trav);
ast({{identifier, _, Name} , {'(', Args}}, {Ctx, Trav}) ->
    call(Name, Args, Ctx, Trav);
ast({{{identifier, _, Name}, Names}, {'(', Args}}, {Ctx, Trav}) ->
    call(Name, Names, Args, Ctx, Trav);  
ast({{identifier, _, Name}, Value}, {Ctx, Trav}) -> 
    var_declare(Name, Value, Ctx, Trav);  
ast({var, DeclarationList}, {Ctx, Trav}) ->     
    {Ast, Inf, Trav1} = p_t(DeclarationList, Ctx#js_ctx{action = set}, Trav),         
    maybe_global({{Ast, Inf}, {Ctx, Trav1}});    
ast(return, {Ctx, Trav}) -> 
    %% TODO: eliminate this clause by adjusting the grammar
    empty_ast(Ctx, Trav);
ast({return, Expression}, {Ctx, Trav}) -> 
    %% TODO: implementation and tests, this doesnt't work generally
    ast(Expression, {Ctx, Trav});
ast({function, {identifier, _L2, Name}, {params, Params, body, Body}}, {Ctx, Trav}) ->
    func(Name, Params, Body, Ctx, Trav);   
ast({op, {Op, _}, In}, {Ctx, Trav}) ->
    {Out, _, #trav{names_set = Set}} = p_t(In, Ctx, Trav),
    {{erlyjs_operators:ast(Op, Out), #ast_inf{}}, {Ctx, Trav#trav{names_set = Set}}};
ast({op, {Op, _}, In1, In2}, {Ctx, Trav}) ->
    {Out1, _, #trav{names_set = Set1}} = p_t(In1, Ctx, Trav),
    {Out2, _, #trav{names_set = Set2}} = p_t(In2, Ctx, Trav#trav{names_set = Set1}),
    {{erlyjs_operators:ast(Op, Out1, Out2), #ast_inf{}}, {Ctx, Trav#trav{names_set = Set2}}};
ast({op, {{Op, postfix}, _}, In1, In2}, {Ctx, Trav}) ->
    %% TODO: fix this, code below is prefix operator
    {Out1, _, #trav{names_set = Set1}} = p_t(In1, Ctx, Trav),
    {Out2, _, #trav{names_set = Set2}} = p_t(In2, Ctx, Trav#trav{names_set = Set1}),
    {{erlyjs_operators:ast(Op, Out1, Out2), #ast_inf{}}, {Ctx, Trav#trav{names_set = Set2}}};
ast({op, Op, In1, In2, In3}, {Ctx, Trav}) ->
    {Out1, _, #trav{names_set = Set1}} = p_t(In1, Ctx, Trav),
    {Out2, _, #trav{names_set = Set2}} = p_t(In2, Ctx, Trav#trav{names_set = Set1}),
    {Out3, _, #trav{names_set = Set3}} = p_t(In3, Ctx, Trav#trav{names_set = Set2}),
    {{erlyjs_operators:ast(Op, Out1, Out2, Out3), #ast_inf{}}, {Ctx, Trav#trav{names_set = Set3}}}; 
ast({assign, {'=', _}, {identifier, _, Name}, In1}, {Ctx, Trav}) ->  
    {{Out2, _}, {_, Trav1}} = var_ast(Name, Ctx#js_ctx{action = set}, Trav),  
    {Out3, Inf, _} = p_t(In1, Ctx, Trav),  
    assign_ast('=', Name, Out2, Out3, Inf, Ctx, Trav1);
ast({assign, {Op, _}, {identifier, _, Name}, In1}, {Ctx, Trav}) ->  
    {{Out2, _}, _} = var_ast(Name, Ctx, Trav),  
    {Out3, Inf, Trav1} = p_t(In1, Ctx, Trav),    
    {{Out4, _}, {_, Trav2}} = var_ast(Name, Ctx#js_ctx{action = set}, Trav1), 
    assign_ast('=', Name, Out4, erlyjs_operators:ast(assign_to_op(Op), Out2, Out3), Inf, Ctx, Trav2);  
ast({'if', Cond, Stmt}, {#js_ctx{global = true} = Ctx, Trav}) -> 
    {Cond2, _, #trav{names_set = Set}} = p_t(Cond, Ctx, Trav),
    Trav2 = Trav#trav{names_set = Set, names = add_names(Trav)},
    {Stmt2, _, Trav3} = p_t(Stmt, Ctx, Trav2),
    ReturnVarsElse = get_vars_init(Trav, Trav3, Ctx),                                        
    Ast = erl_syntax:case_expr(Cond2, [
        erl_syntax:clause([erl_syntax:atom(true)], none, [Stmt2]),
        erl_syntax:clause([erl_syntax:underscore()], none, [ReturnVarsElse])]),
    {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, trav_clean(Trav3)}};    
ast({'if', Cond, Stmt}, {Ctx, Trav}) -> 
    {Cond2, _, #trav{names_set = Set}} = p_t(Cond, Ctx, Trav),
    TravIfIn = Trav#trav{names_set = Set, names = add_names(Trav)},   
    {Stmt2, _, Trav2} = p_t(Stmt, Ctx, TravIfIn),     
    NameKeys = get_name_keys(Trav2),  
    [ReturnVarsIf] = get_vars_list(NameKeys, [Trav2], Trav, Ctx),                 
    ReturnVarsElse = get_vars_init(Trav, Trav2, Ctx),  
    {Vars, Trav3} =  get_vars_result(NameKeys, Trav2, Trav2, Ctx),  
    Ast = erl_syntax:match_expr(Vars, erl_syntax:case_expr(Cond2, [
        erl_syntax:clause([erl_syntax:atom(true)], none, append_asts(Stmt2, ReturnVarsIf)),
        erl_syntax:clause([erl_syntax:underscore()], none, [ReturnVarsElse])])),
    {{Ast, #ast_inf{}}, {Ctx, trav_clean(Trav3)}};    
ast({'ifelse', Cond, StmtIf, StmtElse}, {#js_ctx{global = true} = Ctx, Trav}) ->
    {Cond2, _, #trav{names_set = Set}} = p_t(Cond, Ctx, Trav),
    TravIfIn = Trav#trav{names_set = Set, names = add_names(Trav)},
    {StmtIf2, _, #trav{names_set = Set2} = Trav2} = p_t(StmtIf, Ctx, TravIfIn), 
    TravElseIn = Trav#trav{names_set = Set2, names = add_names(Trav)},
    {StmtElse2, _, Trav3} = p_t(StmtElse, Ctx, TravElseIn),             
    Ast = erl_syntax:case_expr(Cond2, [
        erl_syntax:clause([erl_syntax:atom(true)], none, [StmtIf2]),
        erl_syntax:clause([erl_syntax:underscore()], none, [StmtElse2])]),       
    {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, trav_clean(Trav3)}};    
ast({'ifelse', Cond, StmtIf, StmtElse}, {Ctx, Trav}) ->
    {Cond2, _, #trav{names_set = Set}} = p_t(Cond, Ctx, Trav),
    TravIfIn = Trav#trav{names_set = Set, names = add_names(Trav)},
    {StmtIf2, _, #trav{names_set = Set1} = Trav2} = p_t(StmtIf, Ctx, TravIfIn), 
    TravElseIn = Trav#trav{names_set = Set1, names = add_names(Trav)},
    {StmtElse2, _, Trav3} = p_t(StmtElse, Ctx, TravElseIn),    
    NameKeys = get_name_keys(Trav2, Trav3),     
    [ReturnVarsIf, ReturnVarsElse] = get_vars_list(NameKeys, [Trav2, Trav3], Trav, Ctx),  
    {Vars, Trav4} =  get_vars_result(NameKeys, Trav3, Trav3, Ctx),   
    Ast = erl_syntax:match_expr(Vars, erl_syntax:case_expr(Cond2, [
        erl_syntax:clause([erl_syntax:atom(true)], none, append_asts(StmtIf2, ReturnVarsIf)),
        erl_syntax:clause([erl_syntax:underscore()], none, append_asts(StmtElse2, ReturnVarsElse))])), 
    {{Ast, #ast_inf{}}, {Ctx, trav_clean(Trav4)}};    
ast({do_while, Stmt, Cond}, {#js_ctx{global = true} = Ctx, Trav}) ->   
    {OutStmt, _, Trav2} = p_t(Stmt, Ctx, trav_prepare_func(Trav)),   
    {OutCond, _, Trav3} = p_t(Cond, Ctx, Trav2),
    AstFuncCond = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none,
            [erl_syntax:application(none, func_name(Trav2), [])]),
        erl_syntax:clause([erl_syntax:underscore()], none,  
            [get_global_vars(Trav2)])]),   
    Func = erl_syntax:function(func_name(Trav2),
        [erl_syntax:clause([], none, append_asts(OutStmt, AstFuncCond))]),
    Ast = erl_syntax:application(none, func_name(Trav2), []),
    {{[], #ast_inf{internal_func_asts = [Func], global_asts = [Ast]}}, {Ctx, trav_clean(Trav3)}};
ast({do_while, Stmt, Cond}, {Ctx, Trav}) ->       
    {OutStmt, _, Trav2} = p_t(Stmt, Ctx, trav_prepare_func(Trav)),   
    {OutCond, _, Trav3} = p_t(Cond, Ctx, Trav2),
    VarsBefore = get_vars_init(Trav, Trav2, Ctx),
    VarsAfterStmt = get_vars_snapshot(Trav2),
    {VarsAfter, Trav4} = get_vars_result(Trav2, Trav3, Ctx),    
    AstFuncCond = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none,
            [erl_syntax:application(none, func_name(Trav2), [VarsAfterStmt])]),
        erl_syntax:clause([erl_syntax:underscore()], none,  
            [VarsAfterStmt])]),
    Func = erl_syntax:function(func_name(Trav2),
        [erl_syntax:clause([VarsBefore], none, append_asts(OutStmt, AstFuncCond))]),
    Ast = erl_syntax:match_expr(VarsAfter, 
        erl_syntax:application(none, func_name(Trav2), [VarsBefore])),  
    {{[Ast], #ast_inf{internal_func_asts = [Func]}}, {Ctx, trav_clean(Trav4)}};       
ast({while, Cond, Stmt}, {#js_ctx{global = true} = Ctx, Trav}) ->        
    {OutCond, _, #trav{names_set = Set}} = p_t(Cond, Ctx, Trav),
    Trav2 = Trav#trav{
        names_set = Set, 
        names = add_names(Trav),
        func_counter = wrap_inc_func_counter(Trav)},
    {OutStmt, _, Trav3} = p_t(Stmt, Ctx, Trav2),
    AstFuncCond = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none,
            append_asts(OutStmt, erl_syntax:application(none, func_name(Trav3), []))),
        erl_syntax:clause([erl_syntax:underscore()], none,  
            [get_global_vars(Trav3)])]),    
    Func = erl_syntax:function(func_name(Trav3),
        [erl_syntax:clause([], none, [AstFuncCond])]),
    Ast = erl_syntax:application(none, func_name(Trav3), []),        
    {{[], #ast_inf{internal_func_asts = [Func], global_asts = [Ast]}}, {Ctx, trav_clean(Trav3)}};      
ast({while, Cond, Stmt}, {Ctx, Trav}) ->
    {OutCond, _, #trav{names_set = Set}} = parse_transform(Cond, Ctx, Trav),
    Trav2 = Trav#trav{
        names_set = Set, 
        names = add_names(Trav),
        func_counter = wrap_inc_func_counter(Trav)},
    {OutStmt, _, Trav3} = p_t(Stmt, Ctx, Trav2),
    VarsBefore = get_vars_init(Trav, Trav3, Ctx),
    VarsAfterStmt = get_vars_snapshot(Trav3),
    {VarsAfter, Trav4} = get_vars_result(Trav3, Trav3, Ctx),    
    AstFuncCond = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none,
            append_asts(OutStmt, erl_syntax:application(none, func_name(Trav2), [VarsAfterStmt]))),
        erl_syntax:clause([erl_syntax:underscore()], none,  
            [VarsBefore])]),          
    Func = erl_syntax:function(func_name(Trav2),
        [erl_syntax:clause([VarsBefore], none, [AstFuncCond])]),
    Ast = erl_syntax:match_expr(VarsAfter, 
        erl_syntax:application(none, func_name(Trav3), [VarsBefore])),           
    {{[Ast], #ast_inf{internal_func_asts = [Func]}}, {Ctx, trav_clean(Trav4)}};
ast({for, Init, Cond, Final, Stmt}, {#js_ctx{global = true} = Ctx, Trav}) -> 
    {OutInit, _, Trav2} = p_t(Init, Ctx, Trav),   
    {OutCond, _, Trav3} = p_t(Cond, Ctx, trav_prepare_func(Trav2)),
    {OutStmt, _, Trav4} = p_t(Stmt, Ctx, Trav3),                
    {FinalExpr, _, Trav5} = p_t(Final, Ctx, Trav4),
    Stmts = append_asts(OutStmt, FinalExpr), %%% currently only works with full assignment expression as FinalExpr 
    AstFuncCond = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none,
            append_asts(Stmts, erl_syntax:application(none, func_name(Trav3), []))),
        erl_syntax:clause([erl_syntax:underscore()], none,  
            [get_global_vars(Trav4)])]),   
    Func = erl_syntax:function(func_name(Trav4),
        [erl_syntax:clause([], none, [AstFuncCond])]),
    Ast = erl_syntax:application(none, func_name(Trav3), []),                    
    {{[], #ast_inf{internal_func_asts = [Func], global_asts = append_asts(Ast, OutInit)}}, {Ctx, trav_clean(Trav5)}};
ast({for, Init, Cond, Final, Stmt}, {Ctx, Trav}) -> 
    {OutInit, _, Trav2} = p_t(Init, Ctx, Trav),
    {OutCond, _, Trav3} = p_t(Cond, Ctx, trav_prepare_func(Trav2)),
    {OutStmt, _, Trav4} = p_t(Stmt, Ctx, Trav3),                
    {FinalExpr, _, Trav5} = p_t(Final, Ctx, Trav4),
    VarsBefore = get_vars_init(Trav2, Trav5, Ctx),
    VarsAfterStmt = get_vars_snapshot(Trav5),
    {VarsAfter, Trav6} = get_vars_result(Trav5, Trav5, Ctx),
    Stmts = append_asts(OutStmt, FinalExpr), %%% currently only works with full assignment expression as FinalExpr
    AstFuncCond = erl_syntax:case_expr(OutCond, [
        erl_syntax:clause([erl_syntax:atom(true)], none,
            append_asts(Stmts, erl_syntax:application(none, func_name(Trav3), [VarsAfterStmt]))),
        erl_syntax:clause([erl_syntax:underscore()], none,  
            [VarsBefore])]),
    Func = erl_syntax:function(func_name(Trav3),
        [erl_syntax:clause([VarsBefore], none, [AstFuncCond])]),
    Ast = erl_syntax:match_expr(VarsAfter, 
        erl_syntax:application(none, func_name(Trav5), [VarsBefore])),    
    {{append_asts(OutInit, Ast), #ast_inf{internal_func_asts = [Func]}}, {Ctx, trav_clean(Trav6)}};
ast({switch, Cond, CaseList, {_, DefaultStmts}}, {#js_ctx{global = true} = Ctx, Trav}) -> 
   {Cond2, _, Trav2} = p_t(Cond, Ctx, Trav),   
   CaseList2 = CaseList ++ [{default, DefaultStmts}],
   {List, Trav3} =  get_switch_clause_list(CaseList2, trav_prepare(Trav2), Ctx), 
   Clauses =  lists:map(
       fun
           ({Label, Guard, Stmts, _, _}) -> 
               erl_syntax:clause([Label], Guard, Stmts)
       end, List),                                                               
   Ast = erl_syntax:case_expr(Cond2, Clauses),
   {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, trav_clean(Trav3)}};                   
ast({switch, Cond, CaseList, {_, DefaultStmts}}, {Ctx, Trav}) ->       
    {Cond2, _, Trav2} = p_t(Cond, Ctx, Trav), 
    CaseList2 = CaseList ++ [{default, DefaultStmts}], 
    {List, Trav3} =  get_switch_clause_list(CaseList2, trav_prepare(Trav2), Ctx),
    NameKeys = get_name_keys(List),   
    TravList = [ X || {_,_,_,_,X} <- List],
    StmtsReturnVarsList = get_vars_list(NameKeys, TravList, Trav, Ctx),   
    {Vars, Trav4} =  get_vars_result(NameKeys, Trav3, Trav3, Ctx),     
    Clauses =  lists:map(
        fun
            ({{Label, Guard, Stmts, _, _}, StmtsReturnVars}) ->
                erl_syntax:clause([Label], Guard, append_asts(Stmts, StmtsReturnVars))
        end, lists:zip(List, StmtsReturnVarsList)),                                                              
    Ast = erl_syntax:match_expr(Vars, erl_syntax:case_expr(Cond2, Clauses)),      
    {{Ast, #ast_inf{}}, {Ctx, trav_clean(Trav4)}};
ast(Unknown, _) ->  
    throw({error, lists:concat(["Unknown token: ", Unknown])}). 
    
    
empty_ast(Ctx, Trav) ->
    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, "empty_ast might currently cause exit"]),
    {{[], #ast_inf{}}, {Ctx, Trav}}.  
 
 
func_name(Trav) ->
    erl_syntax:atom(lists:concat(["func_", Trav#trav.func_counter])).
      
      
var_ast(Key, #js_ctx{action = set} = Ctx, Trav) ->
    Scope = hd(Trav#trav.js_scopes),
    ErlName = erl_syntax_lib:new_variable_name(Trav#trav.names_set), 
    Dict = dict:store(Key, ErlName, Scope#scope.names_dict),
    Names = case Trav#trav.names of
        [] ->
            [];
        Val ->
            [dict:store(Key, ErlName, hd(Val)) | tl(Val)]
    end,
    Trav1 = Trav#trav{names_set = sets:add_element(ErlName, Trav#trav.names_set), 
        js_scopes = [#scope{names_dict = Dict} | tl(Trav#trav.js_scopes)],
        names = Names},
    {{erl_syntax:variable(ErlName), #ast_inf{}}, {Ctx, Trav1}}; 
    
var_ast(undefined, #js_ctx{action = get} = Ctx, Trav) ->
    {{erl_syntax:atom(undefined), #ast_inf{}}, {Ctx, Trav}};
var_ast('Infinity', #js_ctx{action = get} = Ctx, Trav) ->
    {{erl_syntax:atom('Infinity'), #ast_inf{}}, {Ctx, Trav}};
var_ast('NaN', #js_ctx{action = get} = Ctx, Trav) ->
    {{erl_syntax:atom('NaN'), #ast_inf{}}, {Ctx, Trav}};        
var_ast(Key, #js_ctx{action = get} = Ctx, Trav) ->
    case name_search(Key, Trav#trav.js_scopes, []) of
        not_found ->
            throw({error, lists:concat(["ReferenceError: ", Key, " is not defined"])});
        {global, Name} ->
            Args = [erl_syntax:atom(Name)], 
            Ast = erl_syntax:application(none, erl_syntax:atom(get), Args),
            {{Ast, #ast_inf{}}, {Ctx, Trav}};
        Name ->
            {{erl_syntax:variable(Name), #ast_inf{}}, {Ctx, Trav}}
    end.

var_declare(Key, [], Ctx, #trav{js_scopes = [GlobalScope]}=Trav) ->      
    Dict = dict:store(Key, global_prefix(Key), GlobalScope#scope.names_dict),
    Args = [erl_syntax:atom(global_prefix(Key)), erl_syntax:atom(undefined)], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args),
    Trav2 = Trav#trav{js_scopes=[#scope{names_dict = Dict}]}, 
    {{Ast,  #ast_inf{}}, {Ctx, Trav2}};    
var_declare(Key, {identifier, _, undefined}, Ctx,  #trav{js_scopes = [_]}=Trav) -> 
    {_, {_, Trav2}}  = var_ast(Key, Ctx, Trav),
    Args = [erl_syntax:atom(global_prefix(Key)), erl_syntax:atom(undefined)], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args),
    {{Ast,  #ast_inf{}}, {Ctx, Trav2}}; 
var_declare(Key, {identifier, _, 'Infinity'}, Ctx,  #trav{js_scopes = [_]}=Trav) -> 
    {_, {_, Trav2}}  = var_ast(Key, Ctx, Trav),
    Args = [erl_syntax:atom(global_prefix(Key)), erl_syntax:atom('Infinity')], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args),
    {{Ast,  #ast_inf{}}, {Ctx, Trav2}};    
var_declare(Key, {identifier, _, 'NaN'}, Ctx,  #trav{js_scopes = [_]}=Trav) -> 
    {_, {_, Trav2}}  = var_ast(Key, Ctx, Trav),
    Args = [erl_syntax:atom(global_prefix(Key)), erl_syntax:atom('NaN')], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args),
    {{Ast,  #ast_inf{}}, {Ctx, Trav2}};           
var_declare(Key, Value, Ctx,  #trav{js_scopes = [GlobalScope]}=Trav) ->      
    Dict = dict:store(Key, global_prefix(Key), GlobalScope#scope.names_dict),
    Trav2 = Trav#trav{js_scopes=[#scope{names_dict = Dict}]},
    {ValueAst, Inf, Trav2} = parse_transform(Value, Ctx, Trav2),
    Args = [erl_syntax:atom(global_prefix(Key)), ValueAst], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args),  
    {{append_asts(Inf#ast_inf.global_asts, Ast),  #ast_inf{}}, {Ctx, Trav2}};  
var_declare(Key, [], Ctx, Trav) ->
    {{AstVariable, _}, {_, Trav2}}  = var_ast(Key, Ctx, Trav),
    Ast = erl_syntax:match_expr(AstVariable, erl_syntax:atom(undefined)),  
    {{Ast, #ast_inf{}}, {Ctx, Trav2}};  
var_declare(Key, {identifier, _, undefined}, Ctx, Trav) ->
    {{AstVar, Inf}, {_, Trav2}}  = var_ast(Key, Ctx, Trav), 
    {{erl_syntax:match_expr(AstVar, erl_syntax:atom(undefined)), Inf}, {Ctx, Trav2}};
var_declare(Key, {identifier, _, 'Infinity'}, Ctx, Trav) ->
    {{AstVar, Inf}, {_, Trav2}}  = var_ast(Key, Ctx, Trav), 
    {{erl_syntax:match_expr(AstVar, erl_syntax:atom('Infinity')), Inf}, {Ctx, Trav2}};
var_declare(Key, {identifier, _, 'NaN'}, Ctx, Trav) ->
    {{AstVar, Inf}, {_, Trav2}}  = var_ast(Key, Ctx, Trav), 
    {{erl_syntax:match_expr(AstVar, erl_syntax:atom('NaN')), Inf}, {Ctx, Trav2}};                        
var_declare(Key, Value, Ctx, Trav) ->
    {{AstVariable, _}, {_, Trav2}}  = var_ast(Key, Ctx, Trav),
    {AstValue, Inf, Trav3} = parse_transform(Value, Ctx, Trav2),
    Ast = erl_syntax:match_expr(AstVariable, AstValue),  
    {{Ast, Inf}, {Ctx, Trav3}}.
    
  
name_search(_, [], _) ->
    not_found;
name_search(Key, [H | T], Trav) ->
    case dict:find(Key, H#scope.names_dict) of
        {ok, Value} ->
            case T of
                [] -> 
                    {global, global_prefix(Key)};
                _ -> Value
            end;
        error -> name_search(Key, T, [H | Trav]) 
    end.
           

get_name_keys(L) when is_list(L) ->            
    lists:usort([Key || {Key, _} <- lists:flatten([element(4, X) || X <- L])]); 

    
get_name_keys(Trav) ->     
    lists:usort([ Key || {Key, _} <- lists:flatten( 
        dict:to_list(hd(Trav#trav.names)))]).    

get_name_keys(Trav1, Trav2) ->     
    lists:usort([ Key || {Key, _} <- lists:flatten(lists:append([
        dict:to_list(hd(Trav1#trav.names)), 
        dict:to_list(hd(Trav2#trav.names))]))]).
    

get_vars_init(Trav1, Trav2, Ctx) ->
    erl_syntax:tuple(dict:fold(
        fun
            (Key, _, AccTravIn) -> 
                {{Ast, _}, {_, _}} = var_ast(Key, Ctx, Trav1),
                [Ast| AccTravIn]
        end, [], hd(Trav2#trav.names))).
            

get_vars_snapshot(Trav) ->                    
    erl_syntax:tuple(dict:fold(
        fun
            (_, Val, AccTravIn) -> 
                [erl_syntax:variable(Val) | AccTravIn]
        end, [], hd(Trav#trav.names))).  
       
        
get_vars_list(NameKeys, TravList, Trav, Ctx) ->        
    lists:map(
      fun
          (X) ->
              erl_syntax:tuple(lists:map(
              fun
                  (Key) ->
                      case  dict:find(Key, hd(X#trav.names)) of
                          {ok, Val} ->
                             erl_syntax:variable(Val);
                          error ->
                             {{Ast, _}, {_, _}} = var_ast(Key, Ctx, Trav),
                             Ast
                      end
              end, NameKeys))
      end, TravList).
        
          
get_global_vars(Trav) ->
    L = dict:to_list(hd(Trav#trav.names)),
    erl_syntax:tuple([erl_syntax:application(none, erl_syntax:atom(get), 
        [erl_syntax:atom(global_prefix(Key))]) || {Key, _} <-  L]).


get_vars_result(Trav, TravSet, Ctx) ->
    get_vars_result(get_name_keys(Trav), Trav, TravSet, Ctx).
    
get_vars_result(NameKeys, Trav, TravSet, Ctx) ->
    TravInit = Trav#trav{names_set = TravSet#trav.names_set},        
    {VarsAfter, Trav2} = lists:mapfoldl(
        fun
            (Key, AccTravIn) ->
                {{Ast, _}, {_, AccTravOut}} = var_ast(Key, Ctx#js_ctx{action = set}, AccTravIn),
                {Ast, AccTravOut}
            end,  TravInit, NameKeys),  
    {erl_syntax:tuple(VarsAfter), Trav2}.      
                

                                       
get_switch_clause_list(CaseList, Trav, Ctx) ->
    lists:mapfoldl(   
        fun      
            ({default, StmtsIn}, AccTravIn) ->  
                AccTravIn2 = trav_reset(AccTravIn),
                LabelOut = erl_syntax:underscore(),
                {StmtsOut, _, AccTravOut} = p_t(StmtsIn, Ctx, AccTravIn2), 
                Names = dict:to_list(hd(AccTravOut#trav.names)),               
                {{LabelOut, none, StmtsOut, Names, AccTravOut}, AccTravOut};                
            ({[LabelIn], StmtsIn}, AccTravIn) -> 
                AccTravIn2 = trav_reset(AccTravIn),
                case lists:last(StmtsIn) of    
                    {break, _} ->                       
                        StmtsIn2 = lists:reverse(tl(lists:reverse(StmtsIn))), 
                        {LabelOut, _, _} = p_t(LabelIn, Ctx, AccTravIn2),   
                        {StmtsOut, _, AccTravOut} = p_t(StmtsIn2, Ctx, AccTravIn2),
                        Names = dict:to_list(hd(AccTravOut#trav.names)),                                       
                        {{LabelOut, none, StmtsOut, Names, AccTravOut}, AccTravOut};
                     _ ->
                         exit(not_implemented_yet) 
                end;
            ({LabelsIn, StmtsIn}, AccTravIn) ->     
                AccTravIn2 = trav_reset(AccTravIn),
                case lists:last(StmtsIn) of    
                    {break, _} ->                       
                        StmtsIn2 = lists:reverse(tl(lists:reverse(StmtsIn))),       
                        {LabelsOut, _, _} = p_t(LabelsIn, Ctx, AccTravIn2),     
                        Guards = erl_syntax:disjunction(lists:map(
                            fun
                                (Label) -> 
                                    Ast = erl_syntax:variable("X"),
                                    erl_syntax:infix_expr(Ast, erl_syntax:operator('=='), Label)
                            end, LabelsOut)),  
                        {StmtsOut, _, AccTravOut} = p_t(StmtsIn2, Ctx, AccTravIn2),  
                        Names = dict:to_list(hd(AccTravOut#trav.names)),          
                        {{erl_syntax:variable("X"), Guards, StmtsOut, Names, AccTravOut}, AccTravOut};
                     _ ->
                         exit(not_implemented_yet) 
                end         
        end, Trav, CaseList).                      
            
                       
func(Name, Params, Body, Ctx, Trav) -> 
    {Params1, _, _} = p_t(Params, Ctx, Trav),          
    {Ast, Inf, _} = p_t(Body, Ctx, wrap_add_scope(Trav)),
    Ast1 = erl_syntax:function(erl_syntax:atom(global_prefix(Name)),
        [erl_syntax:clause(Params1, none, Ast)]),
    case Ctx#js_ctx.global of
        true->
            Export = erl_syntax:arity_qualifier(erl_syntax:atom(global_prefix(Name)), 
                erl_syntax:integer(length(Params))),
            Exports = [Export | Inf#ast_inf.export_asts], 
            {{Ast1, Inf#ast_inf{export_asts = Exports}}, {Ctx, Trav}};
        _ ->
            {{Ast1, Inf}, {Ctx, Trav}}
    end.
    
      
call(Name, Args, Ctx, Trav) ->
    Arity = length(Args),
    case built_in_global_func(Name, Arity) of
        ok ->     
            {Args2, _, Trav2} = p_t(Args, Ctx, Trav),  
            Ast = erl_syntax:application(erl_syntax:atom(erlyjs_global_funcs), 
                erl_syntax:atom(Name), Args2),
            Ast2 = erl_syntax:case_expr(Ast, [
                erl_syntax:clause([erl_syntax:atom(ok)], none, [erl_syntax:atom(ok)]),
                erl_syntax:clause([erl_syntax:tuple([erl_syntax:atom(ok), 
                    erl_syntax:variable("Val")])], none, [erl_syntax:variable("Val")]),
                erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(error)])]),
            maybe_global({{Ast2, #ast_inf{}}, {Ctx, Trav2}});
        _ ->
            throw({error, lists:concat(["No such global function: ", 
                Name, " (arity: ", Arity, ")"])})
    end.


call(Name, Names, Args, Ctx, Trav) ->
    Arity = length(Args),
    case api_func(Name, Names, Arity) of
        {Module, Function} ->
            {Args1, _, Trav1} = p_t(Args, Ctx, Trav),    
            Ast = erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Function), Args1),
            Ast2 = erl_syntax:case_expr(Ast, [
                erl_syntax:clause([erl_syntax:atom(ok)], none, [erl_syntax:atom(ok)]),
                erl_syntax:clause([erl_syntax:tuple([erl_syntax:atom(ok), 
                    erl_syntax:variable("Val")])], none, [erl_syntax:variable("Val")]),
                erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(error)])]),
            maybe_global({{Ast2, #ast_inf{}}, {Ctx, Trav1}});
        _ ->
            throw({error, lists:concat(["No such function: ", todo_prettyprint_functionname])})
    end.
        

built_in_global_func(decodeURI, 1) -> ok;
built_in_global_func(decodeURIComponent, 1) -> ok;
built_in_global_func(encodeURI, 1) -> ok;
built_in_global_func(encodeURIComponent, 1) -> ok;
built_in_global_func(eval, 1) -> ok;
built_in_global_func(eval, 2) -> ok;
built_in_global_func(isFinite, 1) -> ok;
built_in_global_func(isNaN, 1) -> ok;
built_in_global_func(parseInt, 1) -> ok;
built_in_global_func(parseInt, 2) -> ok;
built_in_global_func(parseFloat, 1) -> ok;
built_in_global_func(_, _) -> false.

api_func(console = Name, [log = Func], 1)  -> build_api_func(Name, Func);
api_func(_, _, _) -> false.    

build_api_func(Name, Func) -> {lists:concat(["erlyjs_api_", Name]), Func}.
   
 
assign_ast('=', Name, _, Ast2, _, Ctx, #trav{js_scopes = [_]} = Trav) ->
    %% TODO: dynamic typechecking       
    Ast = erl_syntax:application(none, erl_syntax:atom(put), [
        erl_syntax:atom(global_prefix(Name)), Ast2]),
    maybe_global({{Ast, #ast_inf{}}, {Ctx, Trav}});
assign_ast('=', _, Ast1, Ast2, _, Ctx, Trav) ->
    %% TODO: dynamic typechecking  
    Ast = erl_syntax:match_expr(Ast1, Ast2),
    maybe_global({{Ast, #ast_inf{}}, {Ctx, Trav}}); 
assign_ast(Unknown, _, _, _, _, _, _) ->
    throw({error, lists:concat(["Unknown assignment operator: ", Unknown])}).        


maybe_global({{Ast, Inf}, {#js_ctx{global = true} = Ctx, Trav}}) ->
    GlobalAsts = append_asts(Inf#ast_inf.global_asts, Ast),
    {{[], Inf#ast_inf{global_asts = GlobalAsts}}, {Ctx, Trav}};
maybe_global({AstInf, CtxTrav}) ->    
    {AstInf, CtxTrav}.
    
 
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
    

global_prefix(Name) ->
    lists:concat(["js_", Name]).
                  
%% TODO: eliminate 
add_names(Trav) -> [dict:new() | Trav#trav.names].

trav_prepare(Trav) -> Trav#trav{names = [dict:new() | Trav#trav.names]}.   

trav_prepare_func(Trav) -> 
    Trav#trav{
        names = [dict:new() | Trav#trav.names], 
        func_counter = Trav#trav.func_counter + 1}.   
        
trav_clean(Trav) -> Trav#trav{names = tl(Trav#trav.names)}.
    
trav_reset(Trav) -> Trav#trav{names = [dict:new() | tl(Trav#trav.names)]}.
            
%% TODO: eliminate
wrap_inc_func_counter(Trav) -> Trav#trav.func_counter + 1.      
           
wrap_add_scope(Trav) -> Trav#trav{js_scopes = [#scope{} | Trav#trav.js_scopes]}.

                   
trace(Module, Line, Title, Content, Ctx) ->
    case Ctx#js_ctx.verbose of
        true ->
            io:format("TRACE ~p:~p ~p: ~p~n",[Module, Line, Title, Content]);
        _ ->
            ok
    end.