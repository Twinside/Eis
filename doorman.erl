-module( doorman ).

-include_lib("kernel/include/inet.hrl").
-include( "log_level.hrl" ).
-include( "irc_struct.hrl" ).

-define( LOOKING_HOST_MSG, ":*** Looking up your hostname..." ).
-define( HOST_FOUND_MSG, ":*** Found your hostname." ).
-define( HOST_NOT_FOUND_MSG, ":*** Unable to find your hostname, closing connection." ).
-define( BAD_SEQUENCE_MSG, ":***Bad commands sequence." ).
-define( TIME_OUT_MSG, ":*** Time out, closing connection." ).
-define( VALIDATION_MSG, ":*** You've been regitered !" ).


-define( BOUNCER_FUNC, authentification_process).

-export([
		door_loop/3,					% main loop
		authentification_process/3		% authentification procedure
	]).
-export([
		awaiting_sequence/2,
		awaiting_user/3,
		awaiting_nick_or_server/2
	]).

-vsn( p01 ).

door_loop( ServPid, CliBalance, LSocket ) ->
	case gen_tcp:accept( LSocket ) of
		{ok, CliSock} -> 
			spawn( ?MODULE, ?BOUNCER_FUNC, [CliBalance, CliSock] ),
			door_loop( ServPid, CliBalance, LSocket );
		{error, Reason} ->
			irc_log:logEvent( "Failed to accept client " ++ inet:format_error( Reason ) ),
			door_loop( ServPid, CliBalance, LSocket )
	end.

%%
%% A little function to send quickly a notice during identification.
%%
send_notice( CliSock, Message ) ->
	gen_tcp:send( CliSock, "NOTICE AUTH " ++ Message ).


get_hostname_as_string( Address ) ->
	Host = inet:gethostbyaddr( Address ),
	Host#hostent.h_name.

%%
%% A routine to get a packet from a socket.
%%
get_packet( CliSock ) ->
	case gen_tcp:recv( CliSock, 0, 10000 ) of
		{ok, Packet} ->
			Packet;
		{error, etimedout} -> 
	    	send_notice( CliSock, ?TIME_OUT_MSG ),
			{error, "Failed to accept client Time is out."};
		{error, Reason} -> 
	    	{error, inet:format_error( Reason )}
	end.

%%
%% The procedure used when a client is connecting.
%% They have to respect the sequences of commands :
%% PASS / NICK / USER for an user
%% PASS / SERVER for another server
%%
authentification_process( _ServPid, _CliBalance, CliSock ) ->
	send_notice( CliSock, ?LOOKING_HOST_MSG ),
	case inet:peername( CliSock ) of
		{ok, {Address, _}} -> 
	    	Host = get_hostname_as_string( Address ),
			send_notice( CliSock, ?HOST_FOUND_MSG ),
			case awaiting_sequence( CliSock, Host ) of
				{client, _CliPass, _CliNick, _CliUserName, _CliRealName, _CliSubInfo} -> 
	   				%%
	    			%% Register the client by sending messages to ServPid & CliBalance
		   			%% {client, CliNick, Host, CliUserName, ???, CliSubInfo}
					%% and check if Nick doesn't exists.
					%%
	    			send_notice( CliSock, ?VALIDATION_MSG );
				{error, Reason} ->
		   			gen_tcp:close( CliSock ),
	   				irc_log:logmsg( ?LogVerbose, "Failed to accept client " ++ Reason )
			end;
		{error, Reason} ->
	    	send_notice( CliSock, ?HOST_NOT_FOUND_MSG ),
	   		gen_tcp:close( CliSock ),
	    	irc_log:logEvent( "Failed to accept client " ++ inet:format_error( Reason ) )
    end.

awaiting_sequence( CliSock, Host ) ->
	case get_packet( CliSock ) of
		{error, Reason} ->
			{error, Reason};
		Packet ->
			{msg, _, _, Command, _Params, _} = irc:msg_of_string( Packet ),
	    	case Command of
				'PASS' ->
					%%
					%% Important : add a test on the given password
					%% (need a configuration file)
					%%
		    		awaiting_nick_or_server( CliSock, Host );
				_Other ->
		    		send_notice( CliSock, ?BAD_SEQUENCE_MSG ),
					{error, "Bad commands sequence"}
	    	end
	end.


awaiting_nick_or_server( CliSock, Host ) ->
    case get_packet( CliSock ) of
		{error, Reason} ->
			{error, Reason};
		Packet -> 
			{msg, _, _, Command, Params, _} = irc:msg_of_string( Packet ),
			case Command of
				'NICK' ->
					awaiting_user( CliSock, Host, lists:nth( 0, Params ) );
				'SERVER' ->
					%%
					%% In this case, there are probably something to do.
					%%
					irc:logEvent( "A server tried to connect with you. It's not supported yet." ),
					{error, "It was a server"};
				_Other ->
			    	send_notice( CliSock, ?BAD_SEQUENCE_MSG ),
					{error, "Bad commands sequences"}
			end
	end.


awaiting_user( CliSock, Host, Nick ) ->
    case get_packet( CliSock ) of
		{error, Reason} ->
			{error, Reason};
		Packet -> 
			{msg, _, _, Command, Params, Data} = irc:msg_of_string( Packet ),
			case Command of
				'USER' ->
					UserName = lists:nth( 0, Params ),
					Fun = fun( Str, Acc ) -> Str ++ " " ++ Acc end,
					SubInfo = lists:foldl(Fun, "", Data ) ++ "@" ++ Host,
					SendFunction = undefined, %% A definir selon local ou distant, ici forcement distant
					{client, Nick, Host, UserName, SendFunction, SubInfo};
				_Other ->
			    	send_notice( CliSock, ?BAD_SEQUENCE_MSG ),
					{error, "Bad commands sequences"}
			end
	end.

