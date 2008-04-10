-module( irc_test ).

-include( "irc_struct.hrl" ).

-include_lib( "eunit/include/eunit.hrl" ).

-export([ allowance/0 ]).

% Test pour la validité des nick, fonction un poil complexe,
% donc wallou =)
nickvalid_test_( ) ->
[
    ?_assertNot( irc:is_username_valid( "" ) ),
    ?_assertNot( irc:is_username_valid( ":" ) ),
    ?_assertNot( irc:is_username_valid( "&" ) ),
    ?_assertNot( irc:is_username_valid( "#" ) ),
    ?_assertNot( irc:is_username_valid( "nickvraimenttrestrestrestrestrestreslong" ) ),
    ?_assertNot( irc:is_username_valid( "la:la" ) ),
    ?_assertNot( irc:is_username_valid( "la'" ) ),
    ?_assert( irc:is_username_valid( "^__________^" ) ),
    ?_assert( irc:is_username_valid( "nico" ) ),
    ?_assert( irc:is_username_valid( "Twinside`" ) )
].

chanvalid_test_( ) ->
[
    ?_assert( irc:is_channame_valid( "#pouet" ) )
    ,?_assertNot( irc:is_channame_valid( "#&{:" ) )
].

-define( PTXT1, ":efnet.demon.co.uk 372 Twinside :- ~-=   If you discover illegal images of children are available then   =-~\n" ).
-define( PTXT2, "NOTICE AUTH :*** Processing connection to irc.inet.tele.dk\n" ).
-define( PTXT3, ":efnet.demon.co.uk 366 Twinside #jeanmip :End of /NAMES list.\n" ).
-define( PTXT4, "MODE #jeanmip\n" ).
-define( PTXT5, "PRIVMSG #jeanmip :hahaa aoj\n" ).
-define( PTXT6, ":Twinside!~a@cpy94-4-82-233-227-144.fbx.proxad.net PRIVMSG #jeanmip :fuckings noob\n" ).

%
% Seulement pour éviter les problèmes de 'atom not found'
%
allowance( ) ->
    [ 'NOTICE', 'PRIVMSG', 'MODE' ].
%
% Test "débile" test 2 routines de conversion en une seule passe.
% Part du principe que les deux transformations ne doivent jamais perdre
% de données.
%
parseremake_test_( ) ->
[
    ?_assertEqual( ?PTXT1, irc:string_of_msg( irc:msg_of_string( ?PTXT1 ) ) )
    ,?_assertEqual( ?PTXT2, irc:string_of_msg( irc:msg_of_string( ?PTXT2 ) ) )
    ,?_assertEqual( ?PTXT3, irc:string_of_msg( irc:msg_of_string( ?PTXT3 ) ) )
    ,?_assertEqual( ?PTXT4, irc:string_of_msg( irc:msg_of_string( ?PTXT4 ) ) )
    ,?_assertEqual( ?PTXT5, irc:string_of_msg( irc:msg_of_string( ?PTXT5 ) ) )
    ,?_assertEqual( ?PTXT6, irc:string_of_msg( irc:msg_of_string( ?PTXT6 ) ) )
].


pureparse_test_( ) ->
[
    ?_assertEqual( #msg { ircCommand = 'NICK',
                         data = "",
                         sender = "",
                         params = [ "Youpi" ] },

                    irc:msg_of_string( "NICK Youpi\r\n" ) )
].
    
