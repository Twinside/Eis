-module( doorman_test ).

-include_lib( "eunit/include/eunit.hrl" ).

q1_test_(  ) ->
    [
	?_assert( doorman:q1( {msg, undefined, undefined, 'PASS', ["pass"], undefined}, {"host"} ) == {next_state, q2, {"host", "pass"}} ),
	?_assert( doorman:q1( {msg, undefined, undefined, 'NICK', ["nick"], undefined}, {"host"} ) == {next_state, q3, {"host", undefined, "nick"}} )
    ].
