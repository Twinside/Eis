-module( eis_test ).
-include_lib( "eunit/include/eunit.hrl" ).

try_launch( ) ->
    {Rez, _, _} = eis:dlaunch(),
    Rez.
    
% truc bête : on teste
% que le serveur se lance bien
serverlaunch_test_() ->
[
    ?_assertEqual( ok, try_launch() )
].
    
