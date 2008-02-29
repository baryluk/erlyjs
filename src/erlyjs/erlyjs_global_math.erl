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
    abs/1, 
    acos/1,
    asin/1,
    atan/1,
    atan2/2,
    ceil/1,
    cos/1,
    exp/1,
    floor/1,
    log/1,
    max/2,
    min/2,
    pow/2,
    random/0,
    round/1,
    sin/1,
    sqrt/1,
    tan/1]).  
      
  
abs(X) ->
    erlang:abs(X). 

acos(X) ->
    math:acos(X).  

asin(X) ->
    math:asin(X). 

atan(X) ->
    math:atan(X).  

atan2(X, Y) ->
    math:atan2(X, Y). 

ceil(X) ->
    erlang:round(X + 0.5).  

cos(X) ->
    math:cos(X). 

exp(X) ->
    math:exp(X). 

floor(X) ->
    erlang:round(X - 0.5). 

log(X) ->
    math:log(X). 

max(X, Y) ->
    case (X > Y) of
        true -> X;
        _ -> Y 
    end.
    
min(X, Y) ->
    case (X < Y) of
        true -> X;
        _ -> Y 
    end.

pow(X, Y) ->
    math:pow(X, Y). 

random() ->
    random:uniform(). 

round(X) ->
    erlang:round(X). 

sin(X) ->
    math:sin(X).

sqrt(X) ->
    math:sqrt(X).

tan(X) ->
    math:tan(X).