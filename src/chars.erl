-module( chars ).

-vsn( p01 ).

-export([
            is_letter/1
            ,is_digit/1
            ,is_ircspecial/1
        ]).


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

