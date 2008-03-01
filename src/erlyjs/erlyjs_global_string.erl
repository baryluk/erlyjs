%%%-------------------------------------------------------------------
%%% File:      erlyjs_global_string.erl
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
-module(erlyjs_global_string).
-author('rsaccon@gmail.com'). 
 
-export([  
    charAt/1,
    charCodeAt/1,
    concat/1,
    indexOf/2,
    indexOf/3,
    lastIndexOf/2,
    lastIndexOf/3,
    localeCompare/1,
    match/1,
    replace/2,
    search/1,
    slice/1,
    slice/2,
    split/0,
    split/1,
    split/2,
    substr/1,
    substr/2,
    substring/1,
    substring/2, 
    toLocaleLowerCase/0,
    toLocaleUpperCase/0,
    toLowerCase/1,
    toString/0,
    toUpperCase/1,
    valueOf/0]).
     
 
%% Returns the specified character from a string.
charAt(_Index) ->
    exit(not_implemented_yet).
 
    
%% Returns a number indicating the Unicode value of the character at the given index.
charCodeAt(_Index) ->
    exit(not_implemented_yet).
              

%% Combines the text of two or more strings and returns a new string.        
concat(_Strings) ->
    exit(not_implemented_yet).
                                                                     
        
%% Returns the index within the calling String object of the first occurrence of the specified value, 
%% starting the search at fromIndex, or -1 if the value is not found.        
indexOf(String, SearchValue) -> 
    case string:str(String, SearchValue) of
        0 -> -1;
        Val -> Val - 1
    end.

indexOf(_Str, _SearchValue, _FromIndex) ->
    exit(not_implemented_yet).


%% Returns the index within the calling String object of the last occurrence of the specified value, 
%% or -1 if not found. The calling string is searched backward, starting at fromIndex.                    
lastIndexOf(String, SearchValue) ->
    case string:rstr(String, SearchValue) of
        0 -> -1;
        Val -> Val - 1
    end.

lastIndexOf(_String, _SearchValue, _FromIndex) ->
    exit(not_implemented_yet).
        

%% Returns a number indicating whether a reference string comes before or after or is the same as the given string in sort order.                
localeCompare(_Str) ->
    exit(not_implemented_yet).


%% Used to retrieve the matches when matching a string against a regular expression.
match(_Regexp) ->
    exit(not_implemented_yet).


%% Finds a match between a regular expression and a string, and replaces the matched substring with a new substring.
replace(_RegexpOrSubstr, _NewSubStrOrFunction) ->
    exit(not_implemented_yet).   


%% Executes the search for a match between a regular expression and this String object.
search(_Regexp) ->
    exit(not_implemented_yet).
        

%% Extracts a section of a string and returns a new string.
slice(_BeginSlice) ->
    exit(not_implemented_yet). 
    
slice(_BeginSlice, _EndSlice) ->
    exit(not_implemented_yet).
          
          
%%  Splits a String object into an array of strings by separating the string into substrings.
split() ->
    exit(not_implemented_yet).
            
split(_Separator) ->
    exit(not_implemented_yet).

split(_Separator, _Limit) ->
    exit(not_implemented_yet).


%% Returns the characters in a string beginning at the specified location through the specified number of characters.
substr(_Start) ->
    exit(not_implemented_yet).  
    
substr(_Start, _Length) ->
    exit(not_implemented_yet).
    

%% Returns a subset of a String object.
substring(_IndexA) ->
    exit(not_implemented_yet). 
   
substring(_IndexA, _IndexB) ->
    exit(not_implemented_yet).
        

%% The characters within a string are converted to lower case while respecting the current locale. 
%% For most languages, this will return the same as toLowerCase    
toLocaleLowerCase() ->
    exit(not_implemented_yet).
       
    
%% The characters within a string are converted to upper case while respecting the current locale. 
%% For most languages, this will return the same as toUpperCase           
toLocaleUpperCase() ->
   exit(not_implemented_yet).
    
    
%% Returns the calling string value converted to lower case.
toLowerCase(String) when is_list(String) ->
    string:to_lower(String).        


%% Returns a string representing the specified object. 
toString() ->
    exit(not_implemented_yet).
    

%% Returns the calling string value converted to upper case.     
toUpperCase(String) ->
    string:to_upper(String).  
    

%% Returns the primitive value of a String object.
valueOf() ->
    exit(not_implemented_yet).

