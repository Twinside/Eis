-module( com_join_test ).
-include_lib( "eunit/include/eunit.hrl" ).

simple_join_test_( ) ->
tests:test_scenario(
[
     {"cli1", send, "JOIN #cacaprout"}
    ,{"cli1", recv, ["322", "#cacaprout"] }
    ,{"cli1", recv, ["JOIN", "#cacaprout"] }
]).

multicast_join_test_( ) ->
tests:test_scenario
([
     {"cli1", send, "JOIN #cacaprout"}
    ,{"cli1", recv, ["322", "#cacaprout"] }
    ,{"cli1", recv, [":cli1", "JOIN", "#cacaprout"] }
    ,{"cli2", send, ["JOIN #cacaprout"] }
    ,{"cli1", recv, [":cli2", "JOIN", "#cacaprout"] }
    ,{"cli2", recv, ["322", "#cacaprout"] }
    ,{"cli2", recv, [":cli2", "JOIN", "#cacaprout"] }
]).
