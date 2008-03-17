-module( doorman ).

-include_lib("kernel/include/inet.hrl").
-include( "log_level.hrl" ).
-include( "irc_struct.hrl" ).

-define( BOUNCER_FUNC, authentification_process).

-export([
	 door_loop/3,
	 awaiting_sequence/1,
	 awaiting_sequence/2,
	 athentification_process/3
	]).

-vsn( p01 ).

door_loop( ServPid, CliBalance, LSocket ) ->
    case gen_tcp:accept( LSocket ) of
	{ok, CliSock} -> spawn( ?MODULE, ?BOUNCER_FUNC, [CliBalance, CliSock] ),
			 door_loop( ServPid, CliBalance, LSocket );
	{error, Reason} -> irc_log:logmsg(?LogVerbose, "failed to accept client " ++ inet:format_error( Reason ) ),
			   door_loop( ServPid, CliBalance, LSocket )
    end.


athentification_process( _ServPid, _CliBalance, CliSock ) ->
    gen_tcp:send( CliSock, "NOTICE AUTH :*** Looking up your hostname..." ),
    case inet:peername( CliSock ) of
	{ok, {Address, _Port}} -> 
	    CliAddress = Address,
	    Host = inet:gethostbyaddr( CliAddress ),
	    _CliHost = Host#hostent.h_name,
	    gen_tcp:send( CliSock, "NOTICE AUTH :*** Found your hostname" ),
	    case awaiting_sequence( CliSock ) of
		{_CliPass, _CliNick, _CliUserName, _CliRealName, _CliSubInfo} -> 
		    %%
		    %% Register the client by sending messages
		    %% {client, CliNick, CliHost, CliUserName, ???, CliSubInfo}
		    %% and check if Nick doesn't exists.
		    %%
		    gen_tcp:send( CliSock, "NOTICE AUTH :*** You've been regitered" );
		{error, Reason} ->
		    gen_tcp:close( CliSock ),
		    irc_log:logmsg( ?LogVerbose, "failed to accept client " ++ Reason )
	    end;
	{error, Reason} ->
	    gen_tcp:send( CliSock, "NOTICE AUTH :*** Unable to find your hostname, closing connection." ),
	    gen_tcp:close( CliSock ),
	    irc_log:logmsg( ?LogVerbose, "failed to accept client " ++ inet:format_error( Reason ) )
    end.
   
awaiting_sequence( CliSock ) ->
    case gen_tcp:recv( CliSock, 0, 10000 ) of
	{ok, Packet} -> 
	    Message = irc:msg_of_string( Packet ),
	    case Message#msg.ircCommand of
		'PASS' ->
		    awaiting_sequence( CliSock, {lists:nth( 0, Message#msg.params )} );
		_Other ->
		    gen_tcp:send( CliSock, "NOTICE AUTH :*** Wrong sequence of commands." ),
		    {error, "Wrong sequence of commands."}
	    end;
	{error, etimedout} -> 
	    gen_tcp:send( CliSock, "NOTICE AUTH :*** Time out, closing connection." ),
	    gen_tcp:close(CliSock);
	{error, Reason} -> 
	    irc_log:logmsg( ?LogVerbose, "failed to accept client " ++ inet:format_error( Reason ) ),
	    gen_tcp:close( CliSock )
    end.

awaiting_sequence( CliSock, {Pass} ) ->
    case gen_tcp:recv( CliSock, 0, 10000 ) of
	{ok, Packet} -> 
	    Message = irc:msg_of_string( Packet ),
	    case Message#msg.ircCommand of
		'NICK' ->
		    awaiting_sequence( CliSock, {Pass, lists:nth( 0, Message#msg.params )} );
		_Other ->
		    gen_tcp:send( CliSock, "NOTICE AUTH :*** Wrong sequence of commands." ),
		    {error, "Wrong sequence of commands."}
	    end;
	{error, etimedout} -> 
	    gen_tcp:send( CliSock, "NOTICE AUTH :*** Time out, closing connection." ),
	    gen_tcp:close(CliSock);
	{error, Reason} -> 
	    irc_log:logmsg( ?LogVerbose, "failed to accept client " ++ inet:format_error( Reason ) ),
	    gen_tcp:close( CliSock )
    end;

awaiting_sequence( CliSock, {Pass, Nick} ) ->
    case gen_tcp:recv( CliSock, 0, 10000 ) of
	{ok, Packet} -> 
	    Message = irc:msg_of_string( Packet ),
	    case Message#msg.ircCommand of
		'USER' ->
		    {Pass, Nick, lists:nth( 0, Message#msg.params ), "Toto", "RAS"};
		_Other ->
		    gen_tcp:send( CliSock, "NOTICE AUTH :*** Wrong sequence of commands." ),
		    {error, "Wrong sequence of commands."}
	    end;
	{error, etimedout} -> 
	    gen_tcp:send( CliSock, "NOTICE AUTH :*** Time out, closing connection." ),
	    gen_tcp:close(CliSock);
	{error, Reason} -> 
	    irc_log:logmsg( ?LogVerbose, "failed to accept client " ++ inet:format_error( Reason ) ),
	    gen_tcp:close( CliSock )
    end.
