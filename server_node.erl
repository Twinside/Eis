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

launch() ->
	irc_log:basic_init(),
	% load configuration from file
	irc_log:logInfo( "Server Initialization begin" ),
	MaxCli = 10,	% load real constant from conf
	MaxChan = 10,	% load real constant from conf
	CliBalance = (case load_balancer:start_link(client_listener, MaxCli) of
		{ok, CliBal} -> CliBal;
		_ -> irc_log:logFatal( "Cannot start Client's load balancer, abort"),
			halt()
	end),
	%{ok, ChanBalance} = load_balancer:start_link(chan_manager, MaxChan),
	ok.
	
