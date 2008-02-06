%%%-------------------------------------------------------------------
%%% File:      erlyjs_suite.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc       ErlyJS regression test suite
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
-module(erlyjs_testsuite).
-author('rsaccon@gmail.com').

%% API
-export([run/0, test/1]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec () -> Ast::tuple()
%% @doc
%% @end 
%%--------------------------------------------------------------------
run() ->    
    Errs = filelib:fold_files(tests_dir(), "\.js$", true, fun
            (File, Acc) ->
                case test(File, false) of
                    ok -> Acc;
                    {error, Reason} -> [{File, Reason} | Acc]
                end
        end, []),    
    case Errs of
        [] -> {ok, "Success, all regression tests have passed"};
        _ -> {error, Errs}
    end.
    
    
test(Name) ->
    case make:all([load]) of
        up_to_date ->
            test(filename:join([tests_dir(), Name]) ++  ".js", true);
        _ ->
            {error, "ErlyJS library compilation failed"}
    end.
 
	
%%====================================================================
%% Internal functions
%%====================================================================
test(File, Verbose) ->   
	Module = filename:rootname(filename:basename(File)),
	case erlyjs_compiler:compile(File, Module, [{force_recompile, true}, {verbose, Verbose}]) of
	    ok ->
	        ProcessDict = get(),
	        M = list_to_atom(Module),
	        Expected = M:js_test_result(),
	        Args = M:js_test_args(),
	        M:jsinit(),
	        Result = case catch apply(M, js_test, Args) of
	            Expected -> 
	                ok;
	            Other ->
	                {error, "test failed: " ++ Module ++ " Result: " ++ Other}
	        end,
	        M:jsreset(),
	        case get() of
	            ProcessDict ->
	                Result;
	            _ ->
	                {error, "test failed: " ++ Module ++ " (dirty Process Dictionary)"}
	        end;	                
	    Err ->
	       Err 
	end.
	
	
tests_dir() ->
    {file, Ebin} = code:is_loaded(?MODULE),
    filename:join([filename:dirname(filename:dirname(Ebin)), "src", "tests"]).