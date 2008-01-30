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
-export([compile/2, compile/3, compile/4]).


-record(js_context, {
    global = true,   
    args = [],
    api_namespace = [],
    reader = {file, read_file},
    action = set}).
    
-record(ast_info, {
    vars = [],
    export_asts = [],
    global_asts = []}).

-record(scope, {
    names = [],
    names_used_set = sets:new(),
    names_dict = dict:new()}).
        
        
compile(File, Module) ->
   compile(File, Module, {file, read_file}).


compile(File, Module, Reader) ->
    compile(File, Module, Reader, "ebin").

     
compile(File, Module, Reader, OutDir) ->
    case parse(File, Reader) of
        {ok, JsParseTree} ->               
            io:format("TRACE ~p:~p JsParseTree: ~p~n",[?MODULE, ?LINE, JsParseTree]),
            try body_ast(JsParseTree, #js_context{reader = Reader}, [#scope{}]) of
                {AstList, Info, _} ->                
                    Forms = forms(Module, AstList, Info),  
                    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Forms]),
                    compile_forms(OutDir, Forms)                                      
            catch 
                throw:Error -> Error
            end;
        Error ->
            Error
    end.            
        		
	
%%====================================================================
%% Internal functions
%%====================================================================    

scan(File, {Module, Function}) ->
    case catch Module:Function(File) of
        {ok, B} ->
            erlyjs_lexer:string(binary_to_list(B));
        _ ->
            {error, "reading " ++ File ++ " failed "}
    end.


parse(File, Reader) ->
    case scan(File, Reader) of
        {ok, Tokens, _} ->
            erlyjs_parser:parse(Tokens);
        Err ->
            Err
    end.                                             

forms(Module, FuncAsts, Info) ->
    GlobalFuncAst = erl_syntax:function(erl_syntax:atom("run"),
        [erl_syntax:clause([], none, Info#ast_info.global_asts)]),            
    ModuleAst = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    ExportGlobal = erl_syntax:arity_qualifier(erl_syntax:atom("run"), erl_syntax:integer(0)),
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list([ExportGlobal | Info#ast_info.export_asts])]),
    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, GlobalFuncAst | FuncAsts]].
    
 
compile_forms(OutDir, Forms) ->    
    case compile:forms(Forms) of
        {ok, Module1, Bin} ->       
            Path = filename:join([OutDir, atom_to_list(Module1) ++ ".beam"]),
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
    {AstList, Info, Scopes1};
       
body_ast(JsParseTree, Context, Scopes) ->
    {{Ast, Info}, {_, Vars}} = ast(JsParseTree, {Context, Scopes}),
    {Ast, Info, Scopes}.
    
    

ast({integer, L, Value}, {Context, Scopes}) -> 
    {{erl_syntax:integer(Value), #ast_info{}}, {Context, Scopes}};
ast({identifier, L, Name}, {Context, Scopes}) ->  
    name(Name, Context, Scopes);
ast({{identifier, L, Name}, Value}, {Context, Scopes}) ->  
    var_init(Name, Value, Context, Scopes);             
ast({{var,L}, DeclarationList}, {Context, Scopes}) ->
    {Ast, Info, Scopes1} = body_ast(DeclarationList, Context, Scopes),
    {{Ast, Info}, {Context, Scopes1}}; 
ast({return, L}, {Context, Scopes}) -> 
    %% TODO: add grammar rule and logic
    empty_ast(Context, Scopes);
ast({{return, L}, Expression}, {Context, Scopes}) -> 
    %% TODO: evaluate Expression, not just variable, add logic
    ast(Expression, {Context#js_context{action = get}, Scopes});
ast({{function, L1}, {identifier, L2, Name}, {params, Params, body, Body}}, {Context, Scopes}) ->
    func(Name, Params, Body, Context, Scopes);
ast(Unknown, {Context, Scopes}) ->       
    io:format("TRACE ~p:~p Unknown: ~p~n",[?MODULE, ?LINE, Unknown]),
    empty_ast(Context, Scopes).
    
 
empty_ast(Context, Scopes) ->
    {{[], #ast_info{}}, {Context, Scopes}}.  
    
    
name(Name, #js_context{action = set} = Context, [GlobalScope]) ->
    %% global scope varaible setter
    todo;
name(Name, #js_context{action = set} = Context, Scopes) ->
    Current = hd(Scopes),
    Name1 = erl_syntax_lib:new_variable_name(Current#scope.names_used_set),
    Names = [Name1 | Current#scope.names],
    Set = sets:add_element(Name1, Current#scope.names_used_set),
    Dict = dict:store(Name, Name1, Current#scope.names_dict),
    Current2 = #scope{names = Names, names_used_set = Set, names_dict = Dict},
    {{erl_syntax:variable(Name1), #ast_info{}}, {Context, [Current2 | tl(Scopes)]}}; 
name(Name, #js_context{action = get} = Context, Scopes) ->
    case name_search(Name, Scopes, []) of
        undefined ->
            throw({error, lists:concat(["undefined variable: ", Name])});
        {Name1, Scope1} ->
            {{erl_syntax:variable(Name1), #ast_info{}}, {Context, Name1}}
    end.
  
name_search(Name, [], Acc) ->
    undefined;
name_search(Name, [H | T], Acc) ->
    case dict:find(Name, H#scope.names_dict) of
        {ok, Value} ->
            {Value, lists:merge(lists:reverse(Acc), [H | T])};
        error ->
            name_search(Name, T, [H | Acc]) 
    end.
    
    
var_init(Name, Value, Context, Scopes) ->
    %% TODO: Name stuff
    {AstValue, Info, Vars2} = body_ast(Value, Context, Scopes),
    Ast = erl_syntax:match_expr(erl_syntax:variable("Var"), AstValue),  
    {{Ast, #ast_info{}}, {Context, Vars2}}. 
            
            
func(Name, Params, Body, Context, Scopes) ->  
    Name1 = "func_" ++ atom_to_list(Name), 
    {Params1, _, Scopes1} = body_ast(Params, Context, [#scope{} | Scopes]),          
    {Ast, Info, Scopes2} = body_ast(Body, Context#js_context{global = false}, Scopes1),
    Ast1 = erl_syntax:function(erl_syntax:atom(Name1),
        [erl_syntax:clause(Params1, none, Ast)]),
    case Context#js_context.global of
        true->
            Export = erl_syntax:arity_qualifier(erl_syntax:atom(Name1), erl_syntax:integer(length(Params))),
            Exports = [Export | Info#ast_info.export_asts], 
            {{Ast1, Info#ast_info{export_asts = Exports}}, {Context, Scopes}};
        _ ->
            {{Ast1, Info}, {Context, Scopes}}
    end.
   
    
merge_info(Info1, Info2) ->
    #ast_info{
        export_asts = lists:merge(Info1#ast_info.export_asts, Info2#ast_info.export_asts),
        global_asts = lists:merge(Info1#ast_info.global_asts, Info2#ast_info.global_asts)}. 
       
           

%% body_ast({{'LC', _}, List}, Context) ->
%%  {ListAsts, ListInfo} = lists:foldl(fun
%%          (X, {AccAsts, AccInfo}) ->
%%              case body_ast(X, Context#context{scopes = AccInfo#info.scopes}) of
%%                  {Asts, Info} when is_list(Asts) ->  
%%                      {lists:merge(Asts, AccAsts), info_merge(Info, AccInfo)};
%%                  {Ast, Info} ->  
%%                      {[Ast | AccAsts], info_merge(AccInfo, Info)}
%%              end
%%      end,
%%      {[], #info{scopes = Context#context.scopes}},
%%      lists:reverse(List)),
%%  {lists:reverse(ListAsts), ListInfo};        
%%      
%% body_ast({{'SEMI', _}, Val}, Context) ->
%%     body_ast(Val, Context);
%% 
%% body_ast({{'RETURN', _}, Val}, Context) ->
%%     body_ast(Val, Context);
%%             
%% body_ast({{'STRING', _}, Val}, Context) ->
%%     %% TODO: switch to binaries
%%     {erl_syntax:string(Val), #info{}};
%%     
%% body_ast({{'NUMBER', _}, Val}, Context) ->
%%     %% TODO: either integer or float
%%     {erl_syntax:float(Val), #info{}};
%%     
%% body_ast({{'NAME', _}, Name, Attr, Slot}, Context) ->
%%     VarName = var_name(hd(Context#context.scopes), Name),
%%     return(erl_syntax:variable(VarName), #info{}, Context);
%%     
%% body_ast({{'DOT',_}, Name, {{'NAME',_}, Class, Attr, Slot}}, Context) ->
%%     case check_call(Class, Name) of
%%         {Module, Function} ->
%%             {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Function), 
%%                 Context#context.args), #info{}};
%%         _ ->
%%             io:format("TRACE ~p:~p DOT ~p~n",[?MODULE, ?LINE, error]),
%%             {[], #info{}}
%%     end;
%%         
%% body_ast({{'LP', _}, List}, Context) when is_list(List) ->
%%     List1 = lists:reverse(List),    
%%     ArgAsts = lists:foldr(fun
%%             (X, AccAsts) ->
%%                 case body_ast(X, Context) of
%%                     {Asts, _} when is_list(Asts) ->                        
%%                         lists:merge(Asts, AccAsts);
%%                     {Ast, _} ->                        
%%                         [Ast | AccAsts]
%%                 end
%%         end, [], tl(List1)), 
%%     {MethodAst,_} = body_ast(hd(List1), Context#context{args = ArgAsts}),   
%%     return(MethodAst, #info{}, Context);
%%         
%% body_ast({{'STAR', _}, Left, Right}, Context) ->
%%     %% TODO
%%     {AstInnerLeft,InfoLeft} = body_ast(Left, Context),
%%     {AstInnerRight,InfoRight} = body_ast(Right, Context),
%%     Info = info_merge(InfoLeft, InfoRight),
%%     {[AstInnerLeft, erl_syntax:operator("*"), AstInnerRight], Info};            
%%     
%% body_ast({{'FUNCTION', _}, Name, Args, Body}, Context) ->
%%     Scopes = [#scope{} | Context#context.scopes],
%%     {AstInner,Info} = body_ast(Body, Context#context{scopes=Scopes, global = false}),
%%     Args1 = [erl_syntax:variable("Var" ++ atom_to_list(X)) || X <- Args],                   {Params2, _, Vars2} = body_ast(Params, Context, Vars),
%%     FuncName = "func_" ++ atom_to_list(Name),
%%     Ast = erl_syntax:function(erl_syntax:atom(FuncName),
%%         [erl_syntax:clause(Args1, none, AstInner)]),
%%     case Context#context.global of
%%         true->
%%             Export = erl_syntax:arity_qualifier(erl_syntax:atom(FuncName), erl_syntax:integer(length(Args))),
%%             Exports = [Export | Info#info.export_asts], 
%%             {Ast, Info#info{export_asts = Exports}};
%%         _ ->
%%             {Ast, Info}
%%     end;
%%             
%% body_ast({{'ASSIGN', _}, {{'NAME', _}, Name, _, _}, Right}, Context) ->
%%     {AstInner,Info} = body_ast(Right, Context),
%%     {Scope, VarName} = var_name_update(hd(Context#context.scopes), Name),
%%     Ast = erl_syntax:match_expr(erl_syntax:variable(VarName), AstInner),  
%%     return(Ast, Scope, #info{}, Context);
%%         
%% body_ast({{'VAR', _}, List}, Context) when is_list(List) ->
%%     {Asts, Scope} = lists:foldl(fun
%%             ({{'NAME', _}, X, _, _}, {AccAsts, AccScope}) ->          
%%                 {Scope, VarName} = var_name_update(AccScope, X),
%%                 Ast = erl_syntax:match_expr(erl_syntax:variable(VarName), erl_syntax:atom(undefined)),
%%                 {[Ast | AccAsts], Scope}                        
%%         end, {[], hd(Context#context.scopes)}, lists:reverse(List)),
%%     return(Asts, Scope, #info{}, Context);
%%         
%% body_ast({error, Reason}, _) ->
%%     io:format("TRACE ~p:~p Error: ~p~n",[?MODULE, ?LINE, Reason]),
%%     {[], Reason};
%%         
%% body_ast(Other, _) ->
%%     io:format("TRACE ~p:~p Other: ~p~n",[?MODULE, ?LINE, Other]),
%%  {[], #info{}}.
%%  
%%  
%%  
%% return(Ast, Info, Context) ->
%%     Scopes = Context#context.scopes,   
%%     case Context#context.global of
%%         true ->  
%%             {[], Info#info{scopes = Scopes, global_asts = merge_ast(Ast, Info)}};
%%         _ -> 
%%             {Ast, Info#info{scopes = Scopes}}
%%     end.
%%     
%% return(Ast, Scope, Info, Context) ->
%%     Scopes = Context#context.scopes,   
%%     case Context#context.global of
%%         true ->
%%             {[],  #info{scopes = [Scope | tl(Scopes)], global_asts = merge_ast(Ast, Info)}}; 
%%         _ -> 
%%             {Ast, #info{scopes = [Scope | tl(Scopes)]}} 
%%     end.
%%  
%% merge_ast(Ast, #info{global_asts = Asts}) when is_list(Ast) ->
%%     lists:merge(lists:reverse(Ast), Asts);   
%% merge_ast(Ast, #info{global_asts = Asts}) ->
%%     [Ast | Asts].   
         
    
check_call(console, log)  ->
    {erlyjs_api_console, log};
    
check_call(_, _) ->
    error.