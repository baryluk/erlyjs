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
    global_asts = []}).

-record(scope, {
    names_dict = dict:new()}).
    
-record(tree_acc, {
    names_set = sets:new(),
    scopes = [#scope{}]}).
 
 
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
    InitFuncAstBody = case Info#ast_inf.global_asts of
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
        [erl_syntax:list([ExportInit, ExportReset, ExportChecksum | Info#ast_inf.export_asts])]),
    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, InitFuncAst, ResetFuncAst, ChecksumFuncAst | FuncAsts]].
        
 
compile_forms(Forms, Ctx) ->  
    Options = case Ctx#js_ctx.verbose of
        true ->
            [verbose,report_errors,report_warnings];
        _ ->
            []
    end,  
    case compile:forms(Forms, Options) of
        {ok, Module1, Bin} ->           
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
                {XAst, merge_info(XInf, InfAcc)}
        end, #ast_inf{}, AstInfList),
    {lists:flatten(AstList), Inf, Acc1};     
body_ast(JsParseTree, Ctx, Acc) ->
    {{Ast, Inf}, {_, Acc1}} = ast(JsParseTree, {Ctx, Acc}),
    {Ast, Inf, Acc1}.


ast({identifier, _L, true}, {Ctx, Acc}) -> 
    {{erl_syntax:atom(true), #ast_inf{}}, {Ctx, Acc}};
ast({identifier, _L, false}, {Ctx, Acc}) -> 
    {{erl_syntax:atom(false), #ast_inf{}}, {Ctx, Acc}};    
ast({integer, _L, Value}, {Ctx, Acc}) -> 
    {{erl_syntax:integer(Value), #ast_inf{}}, {Ctx, Acc}};
ast({string, _L, Value}, {Ctx, Acc}) -> 
    {{erl_syntax:string(Value), #ast_inf{}}, {Ctx, Acc}}; %% TODO: binary instead of string
ast({{'[', _L},  Value}, {Ctx, Acc}) -> 
    %% TODO: implementation and tests, this just works for empty lists
    {{erl_syntax:list(Value), #ast_inf{}}, {Ctx, Acc}};
ast({identifier, _L, Name}, {Ctx, Acc}) ->  
    var_name(Name, Ctx, Acc);
ast({{identifier, _L, Name}, Value}, {Ctx, Acc}) ->  
    var_init(Name, Value, Ctx, Acc);             
ast({{var, _L}, DeclarationList}, {Ctx, Acc}) ->
    {Ast, Info, Acc1} = body_ast(DeclarationList, Ctx#js_ctx{action = set}, Acc),
    {{Ast, Info}, {Ctx, Acc1}};
ast({return, _L}, {Ctx, Acc}) -> 
    %% TODO: eliminate this clause by adjusting the grammar
    empty_ast(Ctx, Acc);
ast({{return, _L}, Expression}, {Ctx, Acc}) -> 
    %% TODO: implementation and tests, this just works for return ign literals
    ast(Expression, {Ctx, Acc});
ast({{function, _L1}, {identifier, _L2, Name}, {params, Params, body, Body}}, {Ctx, Acc}) ->
    func(Name, Params, Body, Ctx, Acc);      
ast({{{identifier, _L, Name}, MemberList}, {'(', Args}}, {Ctx, Acc}) ->
    call(Name, MemberList, Args, Ctx, Acc);     
ast({op, {Op, _}, In1, In2}, {Ctx, Acc}) ->
    {Out1, _, #tree_acc{names_set = Set1}} = body_ast(In1, Ctx, Acc),
    {Out2, _, #tree_acc{names_set = Set2}} = body_ast(In2, Ctx, Acc#tree_acc{names_set = Set1}),
    {{op_ast(Op, Out1, Out2), #ast_inf{}}, {Ctx, Acc#tree_acc{names_set = Set2}}};
ast({op, Op, In1, In2, In3}, {Ctx, Acc}) ->
    {Out1, _, #tree_acc{names_set = Set1}} = body_ast(In1, Ctx, Acc),
    {Out2, _, #tree_acc{names_set = Set2}} = body_ast(In2, Ctx, Acc#tree_acc{names_set = Set1}),
    {Out3, _, #tree_acc{names_set = Set3}} = body_ast(In3, Ctx, Acc#tree_acc{names_set = Set2}),
    {{op_ast(Op, Out1, Out2, Out3), #ast_inf{}}, {Ctx, Acc#tree_acc{names_set = Set3}}}; 
ast({assign, {'=' = Op, _}, {identifier, _, Name}, In1}, {Ctx, Acc}) ->  
    {{Out1, _}, {_, Acc1}} = var_name(Name, Ctx#js_ctx{action = set}, Acc),  
    {Out2, Inf, _} = body_ast(In1, Ctx, Acc), 
    {{assign_ast(Op, Out1, Out2), Inf}, {Ctx, Acc1}};   
ast({assign, {Op, _}, {identifier, _, Name}, In1}, {Ctx, Acc}) ->  
    {{Out1, _}, _} = var_name(Name, Ctx, Acc),  
    {Out2, Inf, Acc1} = body_ast(In1, Ctx, Acc),    
    {{Out3, _}, {_, Acc2}} = var_name(Name, Ctx#js_ctx{action = set}, Acc1), 
    {{assign_ast('=', Out3, op_ast(assign_to_op(Op), Out1, Out2)), Inf}, {Ctx, Acc2}};    
ast(Unknown, _) ->
    throw({error, lists:concat(["Unknown token: ", Unknown])}). 
empty_ast(Ctx, Acc) ->
    {{[], #ast_inf{}}, {Ctx, Acc}}.  
    
      
var_name(JsName, #js_ctx{action = set} = Ctx, Acc) ->
    Scope = hd(Acc#tree_acc.scopes),
    ErlName = erl_syntax_lib:new_variable_name(Acc#tree_acc.names_set), 
    Dict = dict:store(JsName, ErlName, Scope#scope.names_dict),
    Acc1 = Acc#tree_acc{names_set = sets:add_element(ErlName, Acc#tree_acc.names_set), 
        scopes = [#scope{names_dict = Dict} | tl(Acc#tree_acc.scopes)]},
    {{erl_syntax:variable(ErlName), #ast_inf{}}, {Ctx, Acc1}}; 
var_name(JsName, #js_ctx{action = get} = Ctx, Acc) ->
    case name_search(JsName, Acc#tree_acc.scopes, []) of
        undefined ->
            throw({error, lists:concat(["ReferenceError: ", JsName, " is not defined"])});
        {global, Name} ->
            Args = [erl_syntax:atom(Name)], 
            Ast = erl_syntax:application(none, erl_syntax:atom(get), Args),
            {{Ast, #ast_inf{}}, {Ctx, Acc}};
        Name ->
            {{erl_syntax:variable(Name), #ast_inf{}}, {Ctx, Acc}}
    end.

var_init(JsName, [], Ctx, #tree_acc{scopes = [GlobalScope]}=Acc) ->      
    Dict = dict:store(JsName, erl_name(JsName), GlobalScope#scope.names_dict),
    Args = [erl_syntax:atom(erl_name(JsName)), erl_syntax:atom(declared)], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args),
    Acc1 = Acc#tree_acc{scopes=[#scope{names_dict = Dict}]}, 
    {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, Acc1}}; 
var_init(JsName, Value, Ctx,  #tree_acc{scopes = [GlobalScope]}=Acc) ->      
    Dict = dict:store(JsName, erl_name(JsName), GlobalScope#scope.names_dict),
    Acc2 = Acc#tree_acc{scopes=[#scope{names_dict = Dict}]},
    {ValueAst, Info1, Acc2} = body_ast(Value, Ctx, Acc2),
    Args = [erl_syntax:atom(erl_name(JsName)), ValueAst], 
    Ast = erl_syntax:application(none, erl_syntax:atom(put), Args), 
    Asts = [Ast | Info1#ast_inf.global_asts],
    {{[], Info1#ast_inf{global_asts = Asts}}, {Ctx, Acc2}};  
var_init(Name, [], Ctx, Acc) ->
    {{AstVariable, _}, {_, Acc1}}  = var_name(Name, Ctx, Acc),
    Ast = erl_syntax:match_expr(AstVariable, erl_syntax:atom(declared)),  
    {{Ast, #ast_inf{}}, {Ctx, Acc1}};  
var_init(Name, Value, Ctx, Acc) ->
    {{AstVariable, _}, {_, Acc1}}  = var_name(Name, Ctx, Acc),
    {AstValue, Info, Acc2} = body_ast(Value, Ctx, Acc1),
    Ast = erl_syntax:match_expr(AstVariable, AstValue),  
    {{Ast, Info}, {Ctx, Acc2}}.
    
  
name_search(_, [], _) ->
    undefined;
name_search(Name, [H | T], Acc) ->
    case dict:find(Name, H#scope.names_dict) of
        {ok, Value} ->
            %% Scopes = lists:merge(lists:reverse(Acc), [H | T]),   %%% TODO: remove, or what was this good for ?
            case T of
                [] -> {global, Value};
                _ -> Value
            end;
        error -> name_search(Name, T, [H | Acc]) 
    end.
             
            
func(Name, Params, Body, Ctx, Acc) -> 
    {Params1, _, _} = body_ast(Params, Ctx, Acc),          
    {Ast, Inf, _} = body_ast(Body, Ctx#js_ctx{global = false}, push_new_scope(Acc)),
    Ast1 = erl_syntax:function(erl_syntax:atom(erl_name(Name)),
        [erl_syntax:clause(Params1, none, Ast)]),
    case Ctx#js_ctx.global of
        true->
            Export = erl_syntax:arity_qualifier(erl_syntax:atom(erl_name(Name)), 
                erl_syntax:integer(length(Params))),
            Exports = [Export | Inf#ast_inf.export_asts], 
            {{Ast1, Inf#ast_inf{export_asts = Exports}}, {Ctx, Acc}};
        _ ->
            {{Ast1, Inf}, {Ctx, Acc}}
    end.


    
call(Name, MemberList, Args, Ctx, Acc) ->
    {Ast, Acc2} = case check_call(Name, MemberList) of
        {Module, Function} ->
            {Args1, _, Acc1} = body_ast(Args, Ctx, Acc),    
            {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Function), Args1),
                Acc1};
        _ ->
            io:format("TRACE ~p:~p DOT ~p~n",[?MODULE, ?LINE, error]),
            {[], Acc}
    end,
    case Ctx#js_ctx.global of
        true->
            {{[], #ast_inf{global_asts = [Ast]}}, {Ctx, Acc2}};
        _ ->
            {{Ast, #ast_inf{}}, {Ctx, Acc2}}
    end.

   
op_ast('*' = Op, Ast1, Ast2) ->
   %% TODO: dynamic typechecking
   erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
op_ast('/' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
op_ast('%', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('rem'), Ast2);    
op_ast('+' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
op_ast('-' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
op_ast('<<', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator("bsl"), Ast2);
op_ast('>>', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator("bsr"), Ast2);  
op_ast('<' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
op_ast('>' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
op_ast('<=', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('=<'), Ast2);
op_ast('>=' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
op_ast('==' = Op, Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator(Op), Ast2);
op_ast('!=', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('/='), Ast2);
op_ast('===', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('=:='), Ast2);
op_ast('!==', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('=/='), Ast2);
op_ast('&', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('band'), Ast2);
op_ast('^', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('bxor'), Ast2);
op_ast('|' , Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('bor'), Ast2);
op_ast('&&', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('and'), Ast2);
op_ast('||', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:infix_expr(Ast1, erl_syntax:operator('or'), Ast2);
op_ast(Unknown, _, _) ->    
    throw({error, lists:concat(["Unknown operator: ", Unknown])}).
    
op_ast('cond', Ast1, Ast2, Ast3) ->
    %% TODO: dynamic typechecking    
    erl_syntax:case_expr(Ast1, [
        erl_syntax:clause([erl_syntax:atom(true)], none, [Ast2]),
        erl_syntax:clause([erl_syntax:underscore()], none, [Ast3])]);
op_ast(Unknown, _, _, _) ->    
    throw({error, lists:concat(["Unknown operator: ", Unknown])}).        
 
 
assign_ast('=', Ast1, Ast2) ->
    %% TODO: dynamic typechecking
    erl_syntax:match_expr(Ast1, Ast2);      
assign_ast(Unknown, _, _) ->    
    throw({error, lists:concat(["Unknown assignment operator: ", Unknown])}).        
 
        
merge_info(Info1, Info2) ->
    #ast_inf{
        export_asts = lists:merge(Info1#ast_inf.export_asts, Info2#ast_inf.export_asts),
        global_asts = lists:merge(Info1#ast_inf.global_asts, Info2#ast_inf.global_asts)}. 


assign_to_op(Assign) ->
    list_to_atom(tl(lists:reverse(string:strip(atom_to_list(Assign), both, $')))).
    

erl_name(Name) ->
    lists:concat(["js_", Name]).
            
push_new_scope(Acc) ->
    Acc#tree_acc{scopes = [#scope{} | Acc#tree_acc.scopes]}.
    
               
check_call(console, [log])  ->
    {erlyjs_api_console, log};
check_call(_, _) ->
    error.
    
    
trace(Module, Line, Title, Content, Ctx) ->
    case Ctx#js_ctx.verbose of
        true ->
            io:format("TRACE ~p:~p ~p: ~p~n",[Module, Line, Title, Content]);
        _ ->
            ok
    end.