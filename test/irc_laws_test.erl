-module( irc_laws_test ).
-include_lib( "eunit/include/eunit.hrl" ).
-include( "irc_struct.hrl" ).

-define( SampChan, #chan{ channame = "", userlist=[], foreignusers=[] } ).

tiny_test_( ) ->
[
    ?_assertNot( irc_laws:is_chan_passworded( ?SampChan ) )
    ,?_assertNot( irc_laws:is_chan_inviteonly( ?SampChan ) )
    ,?_assertNot( irc_laws:is_chan_limited( ?SampChan ) )
].
    
