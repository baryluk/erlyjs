%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright  2007 Roberto Saccon
%%% @doc        Helper module for easy application start, stop, reloading , etc.
%%% @reference  See <a href="http://erlycomet.googlecode.com" target="_top">http://erlycomet.googlecode.com</a> for more information
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
-module(erlyjs).
-author('rsaccon@gmail.com').

-export([start/0, demo/0, demo/1, ast/1]).
 
-define(C_NODE_NUMBER, 2).  %% TODO add gen_server to handle State such as C-Node-Number

%% EOF = 0,                        /* end of file */
%% EOL = 1,                        /* end of line */
%% SEMI = 2,                       /* semicolon */
%% COMMA = 3,                      /* comma operator */
%% ASSIGN = 4,                     /* assignment ops (= += -= etc.) */
%% HOOK = 5, COLON = 6,            /* conditional (?:) */
%% OR = 7,                         /* logical or (||) */
%% AND = 8,                        /* logical and (&&) */
%% BITOR = 9,                      /* bitwise-or (|) */
%% BITXOR = 10,                    /* bitwise-xor (^) */
%% BITAND = 11,                    /* bitwise-and (&) */
%% EQOP = 12,                      /* equality ops (== !=) */
%% RELOP = 13,                     /* relational ops (< <= > >=) */
%% SHOP = 14,                      /* shift ops (<< >> >>>) */
%% PLUS = 15,                      /* plus */
%% MINUS = 16,                     /* minus */
%% STAR = 17, DIVOP = 18,          /* multiply/divide ops (* / %) */
%% UNARYOP = 19,                   /* unary prefix operator */
%% INC = 20, DEC = 21,             /* increment/decrement (++ --) */
%% DOT = 22,                       /* member operator (.) */
%% LB = 23, RB = 24,               /* left and right brackets */
%% LC = 25, RC = 26,               /* left and right curlies (braces) */
%% LP = 27, RP = 28,               /* left and right parentheses */
%% NAME = 29,                      /* identifier */
%% NUMBER = 30,                    /* numeric constant */
%% STRING = 31,                    /* string constant */
%% REGEXP = 32,                    /* RegExp constant */
%% PRIMARY = 33,                   /* true, false, null, this, super */
%% FUNCTION = 34,                  /* function keyword */
%% EXPORT = 35,                    /* export keyword */
%% IMPORT = 36,                    /* import keyword */
%% IF = 37,                        /* if keyword */
%% ELSE = 38,                      /* else keyword */
%% SWITCH = 39,                    /* switch keyword */
%% CASE = 40,                      /* case keyword */
%% DEFAULT = 41,                   /* default keyword */
%% WHILE = 42,                     /* while keyword */
%% DO = 43,                        /* do keyword */
%% FOR = 44,                       /* for keyword */
%% BREAK = 45,                     /* break keyword */
%% CONTINUE = 46,                  /* continue keyword */
%% IN = 47,                        /* in keyword */
%% VAR = 48,                       /* var keyword */
%% WITH = 49,                      /* with keyword */
%% RETURN = 50,                    /* return keyword */
%% NEW = 51,                       /* new keyword */
%% DELETE = 52,                    /* delete keyword */
%% DEFSHARP = 53,                  /* #n= for object/array initializers */
%% USESHARP = 54,                  /* #n# for object/array initializers */
%% TRY = 55,                       /* try keyword */
%% CATCH = 56,                     /* catch keyword */
%% FINALLY = 57,                   /* finally keyword */
%% THROW = 58,                     /* throw keyword */
%% INSTANCEOF = 59,                /* instanceof keyword */
%% DEBUGGER = 60,                  /* debugger keyword */
%% XMLSTAGO = 61,                  /* XML start tag open (<) */
%% XMLETAGO = 62,                  /* XML end tag open (</) */
%% XMLPTAGC = 63,                  /* XML point tag close (/>) */
%% XMLTAGC = 64,                   /* XML start or end tag close (>) */
%% XMLNAME = 65,                   /* XML start-tag non-final fragment */
%% XMLATTR = 66,                   /* XML quoted attribute value */
%% XMLSPACE = 67,                  /* XML whitespace */
%% XMLTEXT = 68,                   /* XML text */
%% XMLCOMMENT = 69,                /* XML comment */
%% XMLCDATA = 70,                  /* XML CDATA section */
%% XMLPI = 71,                     /* XML processing instruction */
%% AT = 72,                        /* XML attribute op (@) */
%% DBLCOLON = 73,                  /* namespace qualified name op (::) */
%% ANYNAME = 74,                   /* XML AnyName singleton (*) */
%% DBLDOT = 75,                    /* XML descendant op (..) */
%% FILTER = 76,                    /* XML filtering predicate op (.()) */
%% XMLELEM = 77,                   /* XML element node type (no token) */
%% XMLLIST = 78,                   /* XML list node type (no token) */
%% YIELD = 79,                     /* yield from generator function */
%% ARRAYCOMP = 80,                 /* array comprehension initialiser */
%% ARRAYPUSH = 81,                 /* array push within comprehension */
%% LEXICALSCOPE = 82,              /* block scope AST node label */
%% LET = 83,                       /* let keyword */
%% BODY = 84,                      /* synthetic body of function with
%%                                    destructuring formal parameters */
%% RESERVED,                       /* reserved keyword */
%% LIMIT                           /* domain size */


%%=====================================================================
%%  API Functions
%%=====================================================================
	
start() ->
   CNodeBinPath = filename:join([filename:dirname(code:which(?MODULE)),"..", "priv", "bin", "erlyjs"]),
   Number = integer_to_list(?C_NODE_NUMBER),
   Cookie = atom_to_list(erlang:get_cookie()),
   Node = atom_to_list(node()),
   Port = open_port({spawn, CNodeBinPath ++ " " ++ Number ++ " " ++ Cookie ++ " " ++ Node}, []),
   spawn(fun() -> loop(Port) end). 


demo() ->
    Dir = filename:join([filename:dirname(code:which(?MODULE)),"..", "demo"]),
    demo(filename:join([Dir, "test.js"])).

 
demo(File) ->   
	case file:read_file(File) of
        {ok, B} ->
			ErlAst = transl(ast(B)),
			    io:format("TRACE ~p:~p Erlang AST:~n ~p~n",[?MODULE, ?LINE, ErlAst]),
				ErlAst;
		_ ->
		    io:format("TRACE ~p:~p reading js sourcefile failed~n",[?MODULE, ?LINE])
	end.

 
ast(B) ->
   call_cnode({list_to_atom(binary_to_list(B))}).


%% ============================================
%% Internal functions
%% ============================================

loop(Port) ->
   receive
     Anything ->
       io:format("Received from jserl: ~p~n", [Anything]),
       loop(Port)
   end.


call_cnode(Msg) ->
    Hostname = string:strip(os:cmd("hostname -s"), right, $\n),
    Nodename = "c" ++ integer_to_list(?C_NODE_NUMBER) ++ "@" ++ Hostname,
    {any, list_to_atom(Nodename)} ! {call, self(), Msg},
    receive
		Result ->
	    	Result
    end.


transl({error, Reason}) ->
    io:format("TRACE ~p:~p Error: ~p~n",[?MODULE, ?LINE, Reason]);
	
transl({{'LC', _Loc}, List}) ->
	[transl(X) || X <- lists:reverse(List)];

transl({{'NAME', _Loc}, Val}) -> 
	{var, 1, Val};
				
transl(Other) ->
    io:format("TRACE ~p:~p Other: ~p~n",[?MODULE, ?LINE, Other]),
	Other.
