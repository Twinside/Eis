-module( doorman ).

-include( "log_level.hrl" ).
-include( "irc_struct.hrl" ).


door_loop( CliBalance, LSocket ) ->
	case gen_tcp:accept( LSocket ) of
		{ok, CliSock} -> spawn(?MODULE, , [CliBalance, CliSock] ),
						door_loop( CliBlanace, LSocket );
		{error, Reason} -> irc_log(?LogVerbose, "failed to accept client " ++ Reason),
							door_loop(CliBalance, LSocket)
	end.

