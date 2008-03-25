-module( server_node ).

-behaviour( gen_server ).

-record( srvs,
		{
			supervisor,
			clibal,		% load balancer for client
			chanbal	% load balancer for channels
		} ).

-export([
			is_client_existing/2
		]).

% export for the gen_server
-export([
			init/1,
			start_link/1,
			handle_call/3,
			handle_cast/2,
			handle_info/2,
			terminate/2,
			code_change/3
		]).

-vsn( p01 ).

is_client_existing( _ServerPid, _NickName ) ->
	false.

start_link({Supervisor, CliBalance, ChanBalance}) ->
	gen_server:start_link(?MODULE,
							[#srvs{supervisor=Supervisor,
									clibal = CliBalance,
									chanbal = ChanBalance }],
							[] ).
		
%%
% gen_server implementation
%%
init( State ) ->
	irc_log:logVerbose( "Server node spawned" ),
	{ok, State}.

handle_call( _What, _From, State ) ->
	{noreply, State}.	
	
%
% Different call used by the load balancer.
%
handle_cast( _Command, State ) ->
	{noreply, State}.
	
handle_info(_What, State) ->
	{noreply, State}.

terminate(_Reason,State) ->
	{ok, State}.

code_change(_OldVsn, State,_Extra) ->
	{ok, State}.

