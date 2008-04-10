-module( eis_test ).
-include_lib( "eunit/include/eunit.hrl" ).

% truc bête : on teste
% que le serveur se lance bien
serverlaunch_test_() ->
[
    ?_assertEqual( ok, eis:dlaunch() )
].
    
