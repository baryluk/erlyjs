%%%-------------------------------------------------------------------
%%% File:      erlyjs_global_math.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2008 Roberto Saccon
%%% @doc  
%%%
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
%%% @since 2008-02-17 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlyjs_global_math).
-author('rsaccon@gmail.com'). 
 
-export([
    f_abs/1, 
    f_acos/1,
    f_asin/1,
    f_atan/1,
    f_atan2/2,
    f_ceil/1,
    f_cos/1,
    f_exp/1,
    f_floor/1,
    f_log/1,
    f_max/2,
    f_min/2,
    f_pow/2,
    f_random/0,
    f_round/1,
    f_sin/1,
    f_sqrt/1,
    f_tan/1]).  
      
  
f_abs(X) ->
    abs(X). 

f_acos(X) ->
    math:acos(X).  

f_asin(X) ->
    math:asin(X). 

f_atan(X) ->
    math:atan(X).  

f_atan2(Y, X) ->
    math:atan2(Y, X). 

f_ceil(X) ->
    round(X + 0.5).  

f_cos(X) ->
    math:cos(X). 

f_exp(X) ->
    math:exp(X). 

f_floor(X) ->
    round(X - 0.5). 

f_log(X) ->
    math:log(X). 

f_max(X, Y) ->
    case (X > Y) of
        true -> X;
        _ -> Y 
    end.
    
f_min(X, Y) ->
    case (X < Y) of
        true -> X;
        _ -> Y 
    end.

f_pow(X, Y) ->
    math:pow(X, Y). 

f_random() ->
    random:uniform(). 

f_round(X) ->
    round(X). 

f_sin(X) ->
    math:sin(X).

f_sqrt(X) ->
    math:sqrt(X).

f_tan(X) ->
    math:tan(X).