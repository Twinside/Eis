-module( chars ).

-vsn( p01 ).

-export([
            is_letter/1
            ,is_digit/1
            ,is_ircspecial/1
            ,is_between/3
            ,flat_append/2
            ,cut_endline/1
            ,int_from_string/1
        ]).

%% @doc
%%  Safely transform a string
%%  to an integer.
%% @end
%% @spec int_from_string( Str ) -> Result
%% where
%%      Str = [char()]
%%      Result = error | int()
int_from_string( Str ) ->
    secure_int( Str, 0 ).
    
secure_int( [], N ) -> N;
secure_int( [C|Next], N ) ->
    Digi = is_digit( C ),
    if Digi -> secure_int( Next, N * 10 + (C - $0) );
       true -> error
    end;
secure_int( _, _ ) -> error.
   
%% @doc
%%  Remove the endline marker in a list.
%% @end
%% @spec cut_endline( Txt ) -> Result
%% where
%%      Txt = string()
%%      Result = string()
cut_endline( Txt ) ->
    cutaux( Txt, lists:reverse( Txt ) ).

cutaux(   _ , [$\n, $\r | Next ] ) -> lists:reverse( Next );
cutaux(   _ , [$\r      | Next ] ) -> lists:reverse( Next );
cutaux(   _ , [$\n      | Next ] ) -> lists:reverse( Next );
cutaux( Orig, []            ) -> Orig;
cutaux( Orig, [_| Next]     ) -> cutaux( Orig, Next ).
   

%% @doc
%%  Flatten a list of string and separe them
%%  with a given separator.
%% @end
%% @spec flat_append( Lst, Sep ) -> Result
%% where
%%      Lst = [string()]
%%      Sep = char()
%%      Result = string()
flat_append(     [], _ ) -> "";
flat_append( [Only], _ ) -> Only;
flat_append( [First,Sec], Separator ) ->
    First ++ [Separator | Sec];
flat_append( [First|Next], Separator ) ->
    flat_appendaux( Separator, First, Next ).

flat_appendaux(   _, Acc, []         ) -> Acc;
flat_appendaux( Sep, Acc, [Last]     ) -> Acc ++ [Sep|Last];
flat_appendaux( Sep, Acc, [Obj|Next] ) ->
    flat_appendaux( Sep, Acc ++ [Sep | Obj], Next ).

%% @doc
%%  Check if a value is in an interval.
%% @end
%% @spec is_between( Val, Min, Max ) -> bool
is_between( Val, Min, Max ) ->
    Val >= Min andalso Val =< Max.

%% @doc
%%  Tell if a integer is a valid
%%  ASCII letter. It must be in
%%  [a-z] or [A-Z]
%% @end
%% @spec is_letter( Char ) -> bool
%% where
%%      Char = int()
is_letter( Char ) ->
    is_between( Char, $A, $Z ) orelse
    is_between( Char, $a, $z ).

%% @doc
%%  Tell if a integer is a valid
%%  ASCII number. It must be in
%%  [0-9]
%% @end
%% @spec is_digit( Char ) -> bool
%% where
%%      Char = int()
is_digit( Char ) ->
    is_between( Char, $0, $9 ).

%% @doc
%%  Tell if a integer is a valid
%%  ASCII character which is not a
%%  letter or a digit and valid on IRC.
%% @end
%% @spec is_ircspecial( Char ) -> bool
%% where
%%      Char = int()
is_ircspecial( Char ) ->
    is_between( Char, $[, $` ) orelse
    is_between( Char, ${, $} ).

