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

-define( GLOB1, "+i" ).
-define( GLOB2, "+o" ).
-define( GLOB3, "+w" ).
-define( GLOB4, "+s" ).

codec_mode( Mo, Funca, Funcb ) ->
    case Funca( Mo ) of
       {_, M} -> Funcb(M);
       {_, _, M} -> Funcb(M);
       _ -> error
    end.

mode_get( Mo, Func ) ->
    case Func(Mo) of
        {_, Mode} -> Mode;
        {_,Mode, _} -> Mode;
        _ -> error
    end.

globcodec_mode( Mo ) ->
    codec_mode( Mo,
                (fun(X) -> irc_laws:string_to_globalmode(X) end),
                (fun(X) -> irc_laws:global_mode_to_string(X) end)).

chacodec_mode( Mo ) ->
    codec_mode( Mo,
                (fun(X) -> irc_laws:string_to_usermode(X) end),
                (fun(X) -> irc_laws:chan_mode_to_string(X) end) ).
                    
globmode_check( Str ) ->
    mode_get( Str, (fun(X) -> irc_laws:string_to_globalmode(X) end) ).
     
chanmode_check( Str ) ->
    mode_get( Str, (fun(X) -> irc_laws:string_to_usermode(X) end) ).

transla_test_( ) ->
[
    ?_assert( globmode_check( ?GLOB1 ) >= 1 )
    ,?_assertEqual( ?GLOB1, globcodec_mode( ?GLOB1 ) )
    ,?_assertEqual( ?GLOB2, globcodec_mode( ?GLOB2 ) )
    ,?_assertEqual( ?GLOB3, globcodec_mode( ?GLOB3 ) )
    ,?_assertEqual( ?GLOB4, globcodec_mode( ?GLOB4 ) )
].

chantransla_test_( ) ->
[
    ?_assertEqual( "+h", chacodec_mode( "+h" ) )
    ,?_assertEqual( "+o", chacodec_mode( "+o" ) )
    ,?_assertEqual( "+s", chacodec_mode( "+s" ) )
    ,?_assertEqual( "+n", chacodec_mode( "+n" ) )
].
