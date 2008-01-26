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

-export([compile/2, compile/3, compile/4]).


-record(scope, {
    names = [],
    names_used_set = sets:new(),
    names_dict = dict:new()}).
    
-record(context, {
    global = true,    
    scopes = [#scope{}],
    args = [],
    api_namespace = []}).
    
-record(info, {
    scopes,
    export_asts = [],
    global_asts = []}).


compile(File, Module) ->
   compile(File, Module, {file, read_file}).


compile(File, Module, Reader) ->
    compile(File, Module, Reader, "ebin").

     
compile(File, Module, Reader, OutDir) ->
    case parse(File, Reader) of
        {ok, JsParseTree} ->    
        
            io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, JsParseTree]);
            
            %% case body_ast(JsParseTree, #context{}) of
            %%     {error, _} = Err ->
            %%         Err;        
            %%     {FuncAsts, Info} ->
            %%         GlobalFuncAst = erl_syntax:function(erl_syntax:atom(Function),
            %%             [erl_syntax:clause([], none, Info#info.global_asts)]),
            %%                     
            %%         ModuleAst = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
            %%         
            %%         ExportGlobal = erl_syntax:arity_qualifier(erl_syntax:atom(Function), erl_syntax:integer(0)),
            %%         ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
            %%             [erl_syntax:list([ExportGlobal | Info#info.export_asts])]),
            %% 
            %%         Forms = [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst, GlobalFuncAst | FuncAsts]],
            %%         
            %%         %% io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Forms]),
            %%         io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, erl_syntax:revert(Forms)]),
            %%         
            %%         case compile:forms(Forms) of
            %%             {ok, Module1, Bin} ->       
            %%                 Path = filename:join([OutDir, atom_to_list(Module1) ++ ".beam"]),
            %%                 case file:write_file(Path, Bin) of
            %%                     ok ->
            %%                         code:purge(Module1),
            %%                         case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
            %%                             {module, _} -> ok;
            %%                             _ -> {error, "code reload failed"}
            %%                         end;
            %%                     {error, Reason} ->
            %%                         {error, lists:concat(["beam generation failed (", Reason, "): ", Path])}
            %%                 end;
            %%             error ->
            %%                 {error, "compilation failed"}
            %%         end                   
            %% end;
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
            io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, Tokens]),
            erlyjs_parser:parse(Tokens);
        Err ->
            Err
    end.


body_ast({{'LC', _}, List}, Context) ->
	{ListAsts, ListInfo} = lists:foldl(fun
	        (X, {AccAsts, AccInfo}) ->
	            case body_ast(X, Context#context{scopes = AccInfo#info.scopes}) of
	                {Asts, Info} when is_list(Asts) ->  
	                    {lists:merge(Asts, AccAsts), info_merge(Info, AccInfo)};
	                {Ast, Info} ->  
	                    {[Ast | AccAsts], info_merge(AccInfo, Info)}
	            end
	    end,
	    {[], #info{scopes = Context#context.scopes}},
	    lists:reverse(List)),
	{lists:reverse(ListAsts), ListInfo};    	
    	
body_ast({{'SEMI', _}, Val}, Context) ->
    body_ast(Val, Context);

body_ast({{'RETURN', _}, Val}, Context) ->
    body_ast(Val, Context);
            
body_ast({{'STRING', _}, Val}, Context) ->
    %% TODO: switch to binaries
    {erl_syntax:string(Val), #info{}};
    
body_ast({{'NUMBER', _}, Val}, Context) ->
    %% TODO: either integer or float
    {erl_syntax:float(Val), #info{}};
    
body_ast({{'NAME', _}, Name, Attr, Slot}, Context) ->
    VarName = var_name(hd(Context#context.scopes), Name),
    return(erl_syntax:variable(VarName), #info{}, Context);
    
body_ast({{'DOT',_}, Name, {{'NAME',_}, Class, Attr, Slot}}, Context) ->
    case check_call(Class, Name) of
        {Module, Function} ->
            {erl_syntax:application(erl_syntax:atom(Module), erl_syntax:atom(Function), 
                Context#context.args), #info{}};
        _ ->
            io:format("TRACE ~p:~p DOT ~p~n",[?MODULE, ?LINE, error]),
            {[], #info{}}
    end;
        
body_ast({{'LP', _}, List}, Context) when is_list(List) ->
    List1 = lists:reverse(List),    
    ArgAsts = lists:foldr(fun
            (X, AccAsts) ->
                case body_ast(X, Context) of
                    {Asts, _} when is_list(Asts) ->                        
                        lists:merge(Asts, AccAsts);
                    {Ast, _} ->                        
                        [Ast | AccAsts]
                end
        end, [], tl(List1)), 
    {MethodAst,_} = body_ast(hd(List1), Context#context{args = ArgAsts}),   
    return(MethodAst, #info{}, Context);
        
body_ast({{'STAR', _}, Left, Right}, Context) ->
    %% TODO
    {AstInnerLeft,InfoLeft} = body_ast(Left, Context),
    {AstInnerRight,InfoRight} = body_ast(Right, Context),
    Info = info_merge(InfoLeft, InfoRight),
    {[AstInnerLeft, erl_syntax:operator("*"), AstInnerRight], Info};            
    
body_ast({{'FUNCTION', _}, Name, Args, Body}, Context) ->
    Scopes = [#scope{} | Context#context.scopes],
    {AstInner,Info} = body_ast(Body, Context#context{scopes=Scopes, global = false}),
    Args1 = [erl_syntax:variable("Var" ++ atom_to_list(X)) || X <- Args],
    FuncName = "func_" ++ atom_to_list(Name),
    Ast = erl_syntax:function(erl_syntax:atom(FuncName),
        [erl_syntax:clause(Args1, none, AstInner)]),
    case Context#context.global of
        true->
            Export = erl_syntax:arity_qualifier(erl_syntax:atom(FuncName), erl_syntax:integer(length(Args))),
            Exports = [Export | Info#info.export_asts], 
            {Ast, Info#info{export_asts = Exports}};
        _ ->
            {Ast, Info}
    end;
            
body_ast({{'ASSIGN', _}, {{'NAME', _}, Name, _, _}, Right}, Context) ->
    {AstInner,Info} = body_ast(Right, Context),
    {Scope, VarName} = var_name_update(hd(Context#context.scopes), Name),
    Ast = erl_syntax:match_expr(erl_syntax:variable(VarName), AstInner),  
    return(Ast, Scope, #info{}, Context);
        
body_ast({{'VAR', _}, List}, Context) when is_list(List) ->
    {Asts, Scope} = lists:foldl(fun
            ({{'NAME', _}, X, _, _}, {AccAsts, AccScope}) ->          
                {Scope, VarName} = var_name_update(AccScope, X),
                Ast = erl_syntax:match_expr(erl_syntax:variable(VarName), erl_syntax:atom(undefined)),
                {[Ast | AccAsts], Scope}                        
        end, {[], hd(Context#context.scopes)}, lists:reverse(List)),
    return(Asts, Scope, #info{}, Context);
        
body_ast({error, Reason}, _) ->
    io:format("TRACE ~p:~p Error: ~p~n",[?MODULE, ?LINE, Reason]),
    {[], Reason};
        
body_ast(Other, _) ->
    io:format("TRACE ~p:~p Other: ~p~n",[?MODULE, ?LINE, Other]),
	{[], #info{}}.
	
	
info_merge(Info1, Info2) ->
    #info{
        scopes = Info2#info.scopes,
        export_asts = lists:merge(Info1#info.export_asts, Info2#info.export_asts),
        global_asts = lists:merge(Info1#info.global_asts, Info2#info.global_asts)}.
 
var_name(Scope, Name) ->
    dict:fetch(Name, Scope#scope.names_dict).
            
var_name_update(Scope, Name) ->
    Name1 = erl_syntax_lib:new_variable_name(Scope#scope.names_used_set),
    Names = [Name1 | Scope#scope.names],
    Set = sets:add_element(Name1, Scope#scope.names_used_set),
    Dict = dict:store(Name, Name1, Scope#scope.names_dict),
    {#scope{names = Names, names_used_set = Set, names_dict = Dict}, Name1}.
 
 
return(Ast, Info, Context) ->
    Scopes = Context#context.scopes,   
    case Context#context.global of
        true ->  
            {[], Info#info{scopes = Scopes, global_asts = merge_ast(Ast, Info)}};
        _ -> 
            {Ast, Info#info{scopes = Scopes}}
    end.
    
return(Ast, Scope, Info, Context) ->
    Scopes = Context#context.scopes,   
    case Context#context.global of
        true ->
            {[],  #info{scopes = [Scope | tl(Scopes)], global_asts = merge_ast(Ast, Info)}}; 
        _ -> 
            {Ast, #info{scopes = [Scope | tl(Scopes)]}} 
    end.
 
merge_ast(Ast, #info{global_asts = Asts}) when is_list(Ast) ->
    lists:merge(lists:reverse(Ast), Asts);   
merge_ast(Ast, #info{global_asts = Asts}) ->
    [Ast | Asts].   
         
    
check_call(console, log)  ->
    {erlyjs_api_console, log};
    
check_call(_, _) ->
    error.