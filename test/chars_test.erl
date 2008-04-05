-module( chars_test ).

-include_lib( "eunit/include/eunit.hrl" ).

charsletter_test_( ) ->
[
    ?_assert( chars:is_letter( $A ) )
    ,?_assert( chars:is_letter( $Z ) )
    ,?_assert( chars:is_letter( $a ) )
    ,?_assert( chars:is_letter( $Z ) )
    ,?_assert( chars:is_letter( $d ) )
    ,?_assert( chars:is_letter( $V ) )

    ,?_assert( not chars:is_letter( $0 ) )
    ,?_assert( not chars:is_letter( $5 ) )
    ,?_assert( not chars:is_letter( $9 ) )
].

digit_test_( ) ->
[
    ?_assert( chars:is_digit( $0 ) )
    ,?_assert( chars:is_digit( $1 ) )
    ,?_assert( chars:is_digit( $2 ) )
    ,?_assert( chars:is_digit( $3 ) )
    ,?_assert( chars:is_digit( $4 ) )
    ,?_assert( chars:is_digit( $5 ) )
    ,?_assert( chars:is_digit( $6 ) )
    ,?_assert( chars:is_digit( $7 ) )
    ,?_assert( chars:is_digit( $8 ) )
    ,?_assert( chars:is_digit( $9 ) )
    ,?_assert( not chars:is_digit( $A ) )
    ,?_assert( not chars:is_digit( $e ) )
    ,?_assert( not chars:is_digit( $^ ) )
].

