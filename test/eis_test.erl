-module( eis_test ).
-include_lib( "eunit/include/eunit.hrl" ).

% truc b�te : on teste
% que le serveur se lance bien
serverlaunch_test_() ->
[
    ?_assertEqual( ok, eis:dlaunch() )
].
    
