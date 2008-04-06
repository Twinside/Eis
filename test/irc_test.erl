-module( irc_test ).

-include_lib( "eunit/include/eunit.hrl" ).

nickvalid_test_( ) ->
[
    ?_assert( not irc:is_username_valid( "" ) ),
    ?_assert( not irc:is_username_valid( ":" ) ),
    ?_assert( not irc:is_username_valid( "&" ) ),
    ?_assert( not irc:is_username_valid( "#" ) ),
    ?_assert( not irc:is_username_valid( "nickvraimenttrestrestrestrestrestreslong" ) ),
    ?_assert( irc:is_username_valid( "^__________^" ) ),
    ?_assert( irc:is_username_valid( "nico" ) ),
    ?_assert( irc:is_username_valid( "Twinside`" ) )
].
