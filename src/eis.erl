%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Launching module for the application.
%%	It's also the main supervisor and an application
%%	regarding the OTP design principles.
%% @end
-module( eis ).

-behaviour( application ).
-behaviour( supervisor ).

%
% export for the application behaviour.
%
-export([
			start/2,
			start_phase/3,
			prep_stop/1,
			stop/1,
			config_change/3
		]).

% export for the supervisor
-export([
			init/1,
			dlaunch/0
		]).

-vsn( p01 ).

make_specserv( Module, Func, Args ) ->
	{Module,
		{Module, Func, Args},
		permanent,
		100,
		worker,
		[Module]}.

make_specbalance( Name, Module, Func, Args ) ->
	{Name,
		{Module, Func, Args},
		permanent,
		100,
		supervisor,
		[Module]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Shortcut used to launch the server
%%	during the debuging period.
%% @end
dlaunch() ->
	LogSpec = make_specserv( irc_log, basic_init, [] ),
	{ok, RootSupervisor} = supervisor:start_link( ?MODULE, [LogSpec]),	
	irc_log:logInfo( "Server Initialization begin" ),
	conf_loader:start_link( "eis.conf" ),
	irc_log:logVerbose( "Started configuration process" ),

	MaxCli = conf_loader:get_int_conf( "cli_per_thread" ),
	MaxChan = conf_loader:get_int_conf( "chan_per_thread" ),
	ListeningPort = conf_loader:get_int_conf( "listening_port" ),
	
	ServerNode = make_specserv( server_node, start_link, [RootSupervisor] ),
	{ok, ServerPid} = supervisor:start_child( RootSupervisor, ServerNode ),
	
	CliBalance = make_specbalance( 'CLIBALANCE', load_balancer, start_link,
									[client_listener, start_link, {MaxCli, ServerPid}] ),
	{ok, CliBalPid} = supervisor:start_child( RootSupervisor, CliBalance ),
	irc_log:logVerbose( "Client balance launched" ),
	
	ChanBalance = make_specbalance( 'CHANBALANCE', load_balancer, start_link,
									[chan_manager, start_link, {MaxChan, ServerPid}] ),
	{ok, ChanBalPid} = supervisor:start_child( RootSupervisor, ChanBalance ),
	irc_log:logVerbose( "Chan balance launched" ),
    gen_server:cast( ServerPid, {set_balance, CliBalPid, ChanBalPid } ),
    	
	DoormanNode = make_specserv( doorman, start_link, [ListeningPort, 
													ServerPid, CliBalPid] ),
	{ok, _DoormanPid} = supervisor:start_child( RootSupervisor, DoormanNode ),
	irc_log:logVerbose( "Doorman launched" ),

	irc_log:logInfo( "End of server initialization" ),
	{ok, ServerPid, RootSupervisor}.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Really start the IRC server, parameters are
%%	an application parameter. Return the PID of
%%  the server node
%% @end
%% @see application
%% @spec start( StartType, StartArgs ) -> Result
%% where
%%      Result = {ok, pid()}
start( _StartType, _StartArgs ) ->
    {ok, _, Root} = dlaunch(),
    {ok, Root}.

%% @hidden
init( Args ) ->
	{ok, { { one_for_one, 1000, 3600 },
		   	Args
		}
	}.
%% @hidden
start_phase( _Phase, _StartType, _PhaseArgs ) ->
	ok.

%% @hidden
prep_stop( State ) ->
	State.

%% @hidden
stop( _State ) ->
	ok.

%% @hidden
config_change( _Changed, _New, _Removed ) ->
	ok.

