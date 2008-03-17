-module( server_node ).

-record( srvs,
		{
			clibal,		% load balancer for client
			chanbal	% load balancer for channels
		} ).

-export([
			launch/0
		]).

-vsn( p01 ).

is_client_existing( ServerPid, NickName ) ->
	false.


balanceLaunch( Module, Func, Max, LogInfo ) ->
	case load_balancer:start_link(Module, Func, Max) of
		{ok, Balance} ->
				irc_log:logVerbose("Started " ++ LogInfo ++ " load balancer"),
				Balance;

		_ ->	irc_log:logFatal( "Cannot start " ++ LogInfo ++ " Load balancer, abort"),
				halt()
	end.

	
launch() ->
	irc_log:basic_init(),
	% load configuration from file
	irc_log:logInfo( "Server Initialization begin" ),
	MaxCli = 10,	% load real constant from conf
	MaxChan = 10,	% load real constant from conf
	CliBalance = balanceLaunch( client_listener, start_link, MaxCli, "Client's"),
	ChanBalance = balanceLaunch( chan_manager, start_link, MaxChan, "Chan's"),
	%{ok, ChanBalance} = load_balancer:start_link(chan_manager, MaxChan),
	ok.
	
