%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% <p>The server_node module is a "gen_server" which store all
%%	all the global informations required to be known across the
%%	server.</p>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module( server_node ).

-behaviour( gen_server ).

-record( srvs,
		{
			supervisor,
			clibal,		% load balancer for client
			chanbal,	% load balancer for channels
			clients,	% global list of clients connected to this server.
			chans		% global list of chans on the network.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	tell if a client is currently registered in
%%	in the server.
%% @end
%% @spec is_client_existing( ServerPid, NickName ) -> bool
%% where
%%		ServerPid = pid()
%%		Nickname = string()
%%
is_client_existing( ServerPid, NickName ) ->
	gen_server:call( ServerPid, {client_exists, NickName} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Launch a new server.
%% @end
%%
start_link({Supervisor, CliBalance, ChanBalance}) ->
	gen_server:start_link(?MODULE,
							[#srvs{supervisor=Supervisor,
									clibal = CliBalance,
									chanbal = ChanBalance,
									clients = ets:new( global_clients, [set] ),
									chans = ets:new( global_chans, [set] ) }],
							[] ).
		
%%
% gen_server implementation
%%
init( State ) ->
	irc_log:logVerbose( "Server node spawned" ),
	{ok, State}.


handle_call( {client_exists, Name}, _From, State ) ->
	case ets:lookup( State#srvs.clients, Name ) of
		[] -> {reply, true ,State};
		_ -> {reply, false, State}
	end;
		
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

