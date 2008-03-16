-module( doorman ).

-include( "log_level.hrl" ).
-include( "irc_struct.hrl" ).

-define( BOUNCER_FUNC, authentificationProcess).

door_loop( CliBalance, LSocket ) ->
	case gen_tcp:accept( LSocket ) of
		{ok, CliSock} -> spawn(?MODULE, ?BOUNCER_FUNC, [CliBalance, CliSock] ),
						door_loop( CliBalance, LSocket );
		{error, Reason} -> irc_log:logmsg(?LogVerbose, "failed to accept client " ++ Reason),
							door_loop(CliBalance, LSocket)
	end.


athentificationProcess( CliBalance, CliSock ) ->
	undefined.
