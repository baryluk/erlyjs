%%%-------------------------------------------------------------------
%%% File:      erlyjs_global.erl
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
-module(erlyjs_global).
-author('rsaccon@gmail.com').

-define(SKIP_ENCODE_URI(C), ((C >= $a andalso C =< $z) orelse
                             (C >= $A andalso C =< $Z) orelse
                             (C >= $0 andalso C =< $9) orelse
                             (C =:= $- orelse 
                              C =:= $_ orelse 
                              C =:= $. orelse 
                              C =:= $! orelse 
                              C =:= $~ orelse 
                              C =:= $* orelse 
                              C =:= $' orelse 
                              C =:= $( orelse 
                              C =:= $) orelse                             
                              C =:= $; orelse 
                              C =:= $, orelse 
                              C =:= $/ orelse 
                              C =:= $? orelse 
                              C =:= $: orelse 
                              C =:= $@ orelse 
                              C =:= $& orelse 
                              C =:= $= orelse 
                              C =:= $+ orelse 
                              C =:= $$ orelse
                              C =:= $#))).
                                  
-define(SKIP_ENCODE_URI_COMP(C), ((C >= $a andalso C =< $z) orelse
                                  (C >= $A andalso C =< $Z) orelse
                                  (C >= $0 andalso C =< $9) orelse
                                  (C =:= $- orelse 
                                   C =:= $_ orelse 
                                   C =:= $. orelse 
                                   C =:= $! orelse 
                                   C =:= $~ orelse 
                                   C =:= $* orelse 
                                   C =:= $' orelse 
                                   C =:= $( orelse 
                                   C =:= $)))).

%% API
-export([
    decode_uri/1,
    decode_uri_component/1,
    encode_uri/1,
    encode_uri_component/1,
    eval/1,
    eval/2,
    is_finite/1,
    is_nan/1,
    parse_int/1,
    parse_int/2,
    parse_float/1]).    
    

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec 
%% @doc
%% @end 
%%--------------------------------------------------------------------

decode_uri(_Str) ->
    exit(not_implemented_yet).


decode_uri_component(_Str) ->
    exit(not_implemented_yet).


encode_uri(Str) ->
    {ok, encode(Str, uri)}. 


encode_uri_component(Str) ->
    {ok, encode(Str, uri_component)}.


%% TODO: variable mapping and bindings
eval(Str) when is_list(Str) ->
    case erlyjs_compiler:parse(Str) of
        {ok, JsParseTree} -> 
            try erlyjs_compiler:parse_transform(JsParseTree) of
                {ErlAstList, _, _} ->    
                    L = [erl_syntax:revert(X) || X <- ErlAstList],             
                    case  erl_eval:exprs(L, erl_eval:new_bindings()) of
                        {value, Value, _Bindings} ->
                            {ok, Value};
                        Err ->
                            Err
                    end;
                Err ->
                    Err                                                    
            catch 
                throw:Err ->
                    Err
            end;
        Err ->
            Err
    end.
    
    
eval(Str, _Object) when is_list(Str) ->
    exit(not_implemented_yet).
    
        
%% TODO: check for positive infinity or negative infinity
is_finite(X) ->
    case is_nan(X) of
        {ok, true} ->
            {ok, false};
        _ ->
            {ok, true}
    end.
            
    
    
%% TODO: check for positive infinity or negative infinity    
is_nan('NaN') ->
    {ok, true};
is_nan(X) when is_number(X) ->
    {ok, false};
is_nan(X) ->
    case is_nan(parse_int2(X)) of
        {ok, true} ->
            is_nan(parse_float2(X));
        _ ->
            {ok, false}
    end.

            
parse_int(Str) -> 
    {ok, parse_int2(Str)}.


parse_int(_Str, _Radix) ->
    exit(not_implemented_yet).
        

parse_float(Str) ->
    {ok, parse_float2(Str)}.

    
%%====================================================================
%% Internal functions
%%====================================================================

%% TODO: a lot, this is just the most simple case
parse_int2(Str) ->
    try list_to_integer(Str) of
        Val -> Val
    catch
        error:badarg -> 'NaN'
    end.
        
            
%% TODO: a lot, this is just the most simple case    
parse_float2(X) ->
    try list_to_float(X) of
        Val -> Val
    catch
        error:badarg -> 'NaN'
    end.
    
    
encode([Input], Type) when is_list(Input) or is_binary(Input) ->
    encode(Input, Type);
encode(Input, Type) when is_binary(Input) ->
    encode(Input, Type, 0);
encode(Input, Type) when is_list(Input) ->
    encode(Input, Type, []).
    
    
encode(Input, Type, Index) when is_binary(Input) ->
    case {Input, Type} of
        {<<_:Index/binary, Byte, _/binary>>, uri = Type} when ?SKIP_ENCODE_URI(Byte) ->
            encode(Input, Type, Index + 1);
        {<<_:Index/binary, Byte, _/binary>>, uri_component = Type} when ?SKIP_ENCODE_URI_COMP(Byte) ->
            encode(Input, Type, Index + 1);
        {<<Pre:Index/binary, Hi:4, Lo:4, Post/binary>>, Type} ->
            HiDigit = hexdigit(Hi),
            LoDigit = hexdigit(Lo),
            Code = <<$\%, HiDigit, LoDigit>>,
            process_binary_match(Pre, Code, size(Post), encode(Post, Type, 0));
        {Input, _} ->
            Input
    end;
encode([], _, Acc) ->
    lists:reverse(Acc);
encode([C | Rest], uri = Type, Acc) when ?SKIP_ENCODE_URI(C) ->
    encode(Rest, Type, [C | Acc]);   
encode([C | Rest], uri_component = Type, Acc) when ?SKIP_ENCODE_URI_COMP(C) ->
    encode(Rest, Type, [C | Acc]);
encode([C | Rest], Type, Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    encode(Rest, Type, [hexdigit(Lo), hexdigit(Hi), $\% | Acc]).


hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).


process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.