%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% <p>The doorman module is in charge of two things.</p>
%% <p>First, he's always waiting a connexion from a listening socket on the
%% machine. When a connection uccurs he create a new process.</p>
%% <p>Second, he contain the code of the just spawned process. This code have
%% this tasks : <ul><li>Receive initialisations messages from the client (PASS,
%% NICK, USER as example).</li><li>Register a new client built from the 
%% received %% informations to the rest of the system.</li></ul></p>
%% <p>The state machine have 3 states :
%% <ul><li>q1 for waiting the PASS message or the NICK message (only if the 
%% peer is a client)</li><li>q2 for waiting the NICK message or the SERVER 
%% message</li><li>q3 for waiting USER message</li></ul></p>
%% @end
%%
%% @reference
%% <p>For more information about the gen_fsm behaviour you are invited to visit
%% the erlang site : 
%% <a href="http://www.erlang.org/doc/design_principles/fsm.html">here</a>.</p>
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module( doorman ).

-behaviour( gen_fsm ).

-include_lib("kernel/include/inet.hrl").
-include( "irc_struct.hrl" ).

-export([
		door_loop/3,
		auth_process/3
	]).

%% Callback from gen_fsm
-export([
		init/1,
		handle_event/3,
		handle_sync_event/4,
		q1/2,
		q1/3,
		q2/2,
		q2/3,
		q3/2,
		q3/3,
		terminate/3
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Function to log that a client failed to connecting with us
log_error( Reason ) ->
	irc_log:logEvent( "Failed to accept client " ++ Reason ).

%% Function to log that a client has joined the server
log_ok( Cli ) ->
	irc_log:logEvent( "A client has just joined : " ++ Cli#client.host ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% First part functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% <p>Door_loop is the listening loop. When the server runs, it's this loop 
%% which permits client's connexions. So the function doesn't finish.</p>
%% @end
%%
%% @spec door_loop ( ServPid, CliBalance, LSocket ) -> Result
%% where
%% 		ServPid = pid()
%% 		CliBalance = pid()
%% 		LSocket = socket()
%% 		Result = none()
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
door_loop( ServPid, CliBalance, LSocket ) ->
	case gen_tcp:accept( LSocket ) of
		{ok, CliSock} -> 
			spawn( ?MODULE, auth_process, [ServPid, CliBalance, CliSock] ),
			door_loop( ServPid, CliBalance, LSocket );
		{error, Reason} ->
			log_error( inet:format_error( Reason ) ),
			door_loop( ServPid, CliBalance, LSocket )
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%% Second part functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% <p>Auth_process is in charge to identify a peer who is trying to connect. He
%% verifies that the firsts received commands are ordered.</p><p>The process 
%% will launch a finite state machine for constraint received commands. But 
%% before he try to resolve name of peer.</p>
%% @end
%%
%% @spec auth_process ( ServPid, CliBalance, CliSock ) -> Result
%% where
%% 		ServPid = pid()
%% 		CliBalance = pid()
%% 		LSocket = socket()
%% 		Result = none()
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
auth_process( ServPid, CliBal, CliSock ) ->
	irc:send_init_msg( CliSock, ?LOOKING_HOST_MSG ),
	case inet:peername( CliSock ) of
		{ok, {Address, _}} ->
			{hostent,_,_,_,_,Host} = inet:gethostbyaddr( Address ),
			irc:send_ident_msg( CliSock, ?HOST_FOUND_MSG ),
			{ok, Pid} = gen_fsm:start_link( doorman,
								{ServPid, CliBal, Host}, []),
			auth_loop( Pid, CliSock, ServPid, CliBal ); 
		{error, Reason} ->
	    	irc:send_ident_msg( CliSock, ?HOST_NOT_FOUND_MSG ),
	   		gen_tcp:close( CliSock ),
	    	log_error( inet:format_error( Reason ) )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% <p>Auth_loop will listen the socket for irc_commands. When a command occurs,
%% the associate event is sent to the dedicated state machine.</p><p>The client
%% have 10 seconds to timeout between two commands. We note that all the 
%% logging actions and the socket action are handled by this loop. So the state
%% machine must return a client, or not. In this case, we deduce that is an
%% error in commands sequence.</p>
%% @end
%%
%% @spec auth_loop ( FsmPid, CliSock, ServPid, CliBal ) -> Result
%% where
%% 		FsmPid = pid()
%% 		CliSock = socket()
%% 		ServPid = pid()
%% 		CliBal = pid()
%% 		Result = {error, Reason} | {ok, Nick, UserName, SubInfo}
%% 		Nick = string()
%% 		UserName = string()
%% 		SubInfo = string()
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
auth_loop( FsmPid, CliSock, ServPid, CliBal ) ->
	case gen_tcp:recv( CliSock, 0, 10000 ) of
		{ok, Packet} ->
			IrcMessage = irc:string_to_msg( Packet ),
			case gen_fsm:sync_send_event( FsmPid, IrcMessage ) of
				continue ->
					auth_loop( FsmPid, CliSock, ServPid, CliBal );
				error ->
					gen_tcp:close( CliSock ),
					log_error( "Bad commands sequence." ),
					error;
				{ok, Client} ->
					log_ok( Client ),
					%% @todo Passer le CliSock au reste du système
					ok
			end;
		{error, etimedout} ->
			gen_fsm:send_event( FsmPid, stop ), % We have to stop state machine
	    	irc:send_ident_msg( CliSock, ?TIME_OUT_MSG ),
			gen_tcp:close( CliSock ),
			log_error( "Time is out." );
		{error, Reason} -> 
			gen_tcp:close( CliSock ),
	    	log_error( inet:format_error( Reason ) )
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% The gen_fsm callback %%%%%%%%%%%%%%%%%%%%%%%%%%%

init( Args ) ->
	{ok, q1, Args}.

handle_event( Message, CurrentState, StateData ) ->
	case Message of
		stop ->
			{stop, error, StateData};
		IrcMessage ->
			CurrentState( IrcMessage, StateData )
	end.

handle_sync_event( Message, _From, CurrentState, StateData ) ->
	case Message of
		stop ->
			{stop, error, StateData};
		IrcMessage ->
			CurrentState( IrcMessage, StateData )
	end.

terminate( Reason, _, StateData ) ->
	case Reason of
		normal ->
			{ServPid, CliBal, Client} = StateData,
			%% @todo Envoyer les bons messages pour ajouter le client
			ok;
		error ->
			error
	end.

q1( {msg, _, _, Command, Params, _}, {ServPid, CliBal, Host} ) ->
	case Command of
		'PASS' ->
			Pass = lists:nth(1, Params),
			{next_state, q2, {ServPid, CliBal, Host, Pass}};
		'NICK' -> % Now we know the peer is a simple client
			{next_state, q3, {ServPid, CliBal, Host, undefined}};
		_ -> % Not a valid message to initialise a connection
			{stop, error, undefined}
	end.
			

q1( {msg, _, _, Command, Params, _}, _From, {ServPid, CliBal, Host} ) ->
	case Command of
		'PASS' ->
			Pass = lists:nth(1, Params),
			{reply, continue, q2, {ServPid, CliBal, Host, Pass}};
		'NICK' -> % Now we know the peer is a simple client
			{reply, continue, q3, {ServPid, CliBal, Host, undefined}};
		_ -> % Not a valid message to initialise a connection
			{stop, error, error, undefined}

	end.


q2( {msg, _, _, Command, Params, _}, {ServPid, CliBal, Host, Pass} ) ->
	case Command of
		'PASS' ->
			NewPass = lists:nth(1, Params),
			{next_state, q2, {ServPid, CliBal, Host, NewPass}};
		'NICK' ->
			Nick = lists:nth(1, Params),
			PassResponse = valid,
			%% @todo PassResponse = verifier le Pass / Nick
			case PassResponse of
				valid ->
					{next_state, q3, {ServPid, CliBal, Host, Pass, Nick}}; 
				error ->
					{stop, error, undefined}
			end;
		'SERVER' ->
			{stop, error, undefined}; % NOT SUPPORTED
		_ -> % Not a valid message to initialise a connection
			{stop, error, undefined}
	end.

q2( {msg, _, _, Command, Params, _}, _From, {ServPid, CliBal, Host, Pass} ) ->
	case Command of
		'PASS' ->
			NewPass = lists:nth(1, Params),
			{reply, continue, q2, {ServPid, CliBal, Host, NewPass}};
		'NICK' ->
			Nick = lists:nth(1, Params),
			PassResponse = valid,
			%% @todo PassResponse = verifier le Pass / Nick
			case PassResponse of
				valid ->
					{reply, continue, q3, {ServPid, CliBal, Host, Pass, Nick}}; 
				error ->
					{stop, error, error, undefined}
			end;
		'SERVER' ->
			{stop, error, error, undefined}; % NOT SUPPORTED
		_ -> % Not a valid message to initialise a connection
			{stop, error, error, undefined}
	end.

q3( {msg, _, _, Command, Params, Data},{ServPid, CliBal, Host, Pass, Nick} ) ->
	case Command of
		'USER' ->
			Name = lists:nth(1, Params),
			Send = undefined,
			Client = {client, Nick, Host, Name, Send, Data},
			%% @todo Send = qch ??
			{stop, normal, {ServPid, CliBal, Client}};
		_ -> % Not a valid message to initialise a connection
			{stop, error, error, undefined}
	end.

q3( {msg, _, _, Command, Params, Data}, _From,
                                      {ServPid, CliBal, Host, Pass, Nick} ) ->
	case Command of
		'USER' ->
			Name = lists:nth(1, Params),
			Send = undefined,
			Client = {client, Nick, Host, Name, Send, Data},
			%% @todo Send = qch ??
			{stop, normal, {ok, Client}, {ServPid, CliBal, Client}};
		_ -> % Not a valid message to initialise a connection
			{stop, error, error, undefined}
	end.

