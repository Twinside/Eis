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
		1000,
		worker,
		[Module]}.

make_specbalance( Name, Module, Func, Args ) ->
	{Name,
		{Module, Func, Args},
		permanent,
		1000,
		supervisor,
		[Module]}.

init( Args ) ->
	{ok, { { one_for_one, 1000, 3600 },
		   	Args
		}
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
dlaunch() -> start( 0, 0 ).

start( _StartType, _StartArgs ) ->
	LogSpec = make_specserv( irc_log, basic_init, [] ),
	{ok, RootSupervisor} = supervisor:start_link( ?MODULE, [LogSpec]),	
	irc_log:logInfo( "Server Initialization begin" ),
	MaxCli = 10,	% load real constant from conf
	MaxChan = 10,	% load real constant from conf
	CliBalance = make_specbalance( 'CLIBALANCE', load_balancer, start_link,
									[client_listener, start_link, MaxCli] ),
	{ok, CliBalPid} = supervisor:start_child( RootSupervisor, CliBalance ),
	irc_log:logVerbose( "Client balance launched" ),
	ChanBalance = make_specbalance( 'CHANBALANCE', load_balancer, start_link,
									[chan_manager, start_link, MaxChan] ),
	{ok, ChanBalPid} = supervisor:start_child( RootSupervisor, ChanBalance ),
	irc_log:logVerbose( "Chan balance launched" ),
	ServerNode = make_specserv( server_node, start_link,
								[{RootSupervisor, CliBalPid, ChanBalPid}] ),
	{ok, _ServerPid} = supervisor:start_child( RootSupervisor, ServerNode ),
	irc_log:logInfo( "End of server initialization" ),
	ok.

start_phase( _Phase, _StartType, _PhaseArgs ) ->
	ok.

prep_stop( State ) ->
	State.

stop( _State ) ->
	ok.

config_change( _Changed, _New, _Removed ) ->
	ok.

