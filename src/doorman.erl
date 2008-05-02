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
%% <ul><li>init_state for waiting the PASS message or the NICK message (only if the
%% peer is a client)</li><li>q2 for waiting the NICK message or the SERVER
%% message</li><li>final_registration_state for waiting USER message</li></ul></p>
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
-include( "irc_authstrings.hrl" ).

-export([
            door_loop/3
            ,auth_process/3
            ,start_link/3
	    ]).

%% Callback from gen_fsm
-export([
            init/1
            ,handle_event/3
            ,handle_sync_event/4
            ,terminate/3

            ,code_change/4
            ,handle_info/3
	    ]).

-export([
            init_state/3
            ,q2/3
            ,final_registration_state/3
        ]).

-define( IDENT_TIMEOUT, 6 ).

%-define( STATE_DEBUG, {debug, [trace] } ).
-define( STATE_DEBUG,  ).

%-define( LOCAL_SEND, (fun({local, Sock}, Tosend) -> gen_tcp:send( Sock, Tosend )end)).
-define( LOCAL_SEND, (fun({local, Sock}, Tosend) -> irc_log:logDebug( "----> " ++ Tosend ++ "\n"),
                                                    gen_tcp:send( Sock, Tosend )end)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Function to log that a client failed to connecting with us
log_error( Reason ) ->
	irc_log:logEvent( "Failed to accept client " ++ Reason ).

%% Function to log that a client has joined the server
log_ok( Cli ) ->
	irc_log:logEvent( "A client has just joined : " ++ Cli#client.nick ++ "@"
							++ Cli#client.host ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% To squeeze warnings  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_change( _, _, _, _ ) -> undefined.
handle_info( _, _, _ ) -> undefined.
handle_event( _, _, _ ) -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% First part functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% <p>The launching funtion for the doorman. She launch the main loop.</p>
%% @end
%%
%% @spec start_link ( ServPid, CliBalance, LSocket ) -> Result
%% where
%% 		ServPid = pid()
%% 		Result = {ok, pid()}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link( Port, ServPid, CliBalance ) ->
	{ok, LSocket} = gen_tcp:listen(Port, [{active, false}, {packet, line},
							{reuseaddr, true}]),
	Pid = spawn(?MODULE, door_loop, [ServPid, CliBalance, LSocket]),
    ok = gen_tcp:controlling_process( LSocket, Pid ),
	{ok, Pid}.

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
			Pid = spawn( ?MODULE, auth_process, [ServPid, CliBalance, CliSock] ),
            ok = gen_tcp:controlling_process( CliSock, Pid ),
			door_loop( ServPid, CliBalance, LSocket );
		{error, Reason} ->
			log_error( inet:format_error( Reason ) ),
			door_loop( ServPid, CliBalance, LSocket )
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%% Second part functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record( auth,
        {
            host = ""   %% as string
            ,server     %% as pid()
            ,sock       %% as socket()
            ,nick = ""  %% as string()
            ,pass = ""  %% as string()
            ,user = ""  %% as string()
        }).
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
	irc:send_ident_msg( CliSock, ?LOOKING_HOST_MSG ),
	inet:setopts(CliSock, [{active, false}, {packet, line}]),

	case inet:peername( CliSock ) of
		{ok, {Address, _}} ->
			{ok , {hostent,Host,_,_,_,_}} = inet:gethostbyaddr( Address ),
			irc:send_ident_msg( CliSock, ?HOST_FOUND_MSG ),
			irc:send_ident_msg( CliSock, ?CHECKING_IDENT ),
			case ident_checker:check_ident( CliSock, ?IDENT_TIMEOUT ) of
			    {error, _Reason} ->
				irc:send_ident_msg( CliSock, ?IDENT_CHECK_FAIL );
			    {ok, {_System, _Data}} ->
				irc:send_ident_msg( CliSock, ?IDENT_CHECK_VALID )
			end,
			State = #auth { host = Host
                            ,server = ServPid
                            ,sock = CliSock },
			{ok, Pid} = gen_fsm:start_link( doorman, State, [?STATE_DEBUG]),
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
	case gen_tcp:recv( CliSock, 0 ) of %, 100000 ) of
		{ok, Packet} ->
			IrcMessage = irc:msg_of_string( Packet ),
			case gen_fsm:sync_send_event( FsmPid, IrcMessage, 100000 ) of
				continue ->
					auth_loop( FsmPid, CliSock, ServPid, CliBal );

				error ->
					gen_tcp:close( CliSock ),
					irc:send_ident_msg( CliSock, ?BAD_SEQUENCE_MSG ),
					log_error( "Bad commands sequence." ),
					error;

				{ok, {Client, _Auth}} ->
					% @todo check client password
				    Send = ?LOCAL_SEND,
				    RealClient = Client#client{ send = Send
                                             ,sendArgs = {local, CliSock} },
				    irc:send_ident_msg( CliSock, ?VALIDATION_MSG ),
				    server_node:add_user( ServPid, RealClient ),
				    log_ok( Client ),
				    ok
			end;
		{error, etimedout} ->
			gen_fsm:send_event( FsmPid, stop ), % We have to stop state machine
	    	irc:send_ident_msg( CliSock, ?TIME_OUT_MSG ),
			gen_tcp:close( CliSock ),
			log_error( "Client registration timed out." );
		{error, Reason} ->
			gen_tcp:close( CliSock ),
	    	log_error( inet:format_error( Reason ) )
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% The gen_fsm callback %%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
init( Args ) -> {ok, init_state, Args}.

%% @hidden
handle_sync_event( stop, _From, _CurrentState, StateData ) ->
    {stop, error, StateData};
handle_sync_event( IrcMessage, _From, CurrentState, StateData ) ->
    CurrentState( IrcMessage, 0, StateData ).

%% @hidden
terminate( normal, _, _ ) -> ok;
terminate( error, _, _ ) -> error.

%% @hidden
init_state( Msg, _From, Auth ) ->
	case Msg#msg.ircCommand of
		'PASS' ->
			[Pass|_] = Msg#msg.params,
			{reply, continue, q2, Auth#auth{ pass = Pass} };
		'NICK' -> valid_nick( Msg, Auth );
		_ -> % Not a valid message to initialise a connection
			{stop, error, error, undefined}

	end.

%% @hidden
q2( Msg, _From, Auth ) ->
	case Msg#msg.ircCommand of
		'PASS' ->
			[NewPass|_] = Msg#msg.params,
			{reply, continue, q2, Auth#auth{ pass = NewPass } };
		'NICK' -> valid_nick( Msg, Auth );
		'SERVER' -> {stop, error, error, undefined}; % NOT SUPPORTED
		_ -> % Not a valid message to initialise a connection
			{stop, error, error, undefined}
	end.

valid_nick( Msg, Auth ) ->
    [Nick|_] = Msg#msg.params,
    Used = server_node:is_client_existing( Auth#auth.server, Nick ),
    if Used -> irc:send_ident_msg( Auth#auth.sock, ?NICK_ALREADY_USED ),
                % TODO : renvoyer le vrai message d'erreur
                %       ERR_NICKNAMEINUSE...
                {reply, continue, q2, Auth};

       true -> {reply, continue, final_registration_state, Auth#auth{ nick = Nick } }
    end.

%% @hidden
final_registration_state( Msg, _From, Auth ) ->
	case Msg#msg.ircCommand of
		'USER' ->
			[Name|_] = Msg#msg.params,
			Client = #client{ nick = Auth#auth.nick
                              ,host = Auth#auth.host
                              ,username = Name },
			{stop, normal, {ok, {Client, Auth#auth{user=Name} }
                            },
                            undefined
             };
		_ -> % Not a valid message to initialise a connection
			{stop, error, error, undefined}
	end.

