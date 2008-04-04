%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% <p>The server_node module is a "gen_server" which store all
%%	all the global informations required to be known across the
%%	server.</p>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module( server_node ).

-behaviour( gen_server ).
-include( "irc_struct.hrl" ).


-export([
			is_client_existing/2,
			is_chan_existing/2,
			get_client/2,
			get_chan/2,
			add_chan/2,
			add_user/2
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

%% @doc
%%	tell if a chan with the given name exist in the server.
%% @end
%% @spec is_chan_existing( ServerPid, NickName ) -> bool
%% where
%%		ServerPid = pid()
%%		Nickname = string()
is_chan_existing( ServerPid, NickName ) ->
	gen_server:call( ServerPid, {chan_exists, NickName} ).

%% @doc
%%	Retrieve a client from the server node.
%% @end
%% @spec get_client( ServerPid, NickName ) -> Result
%% where
%%		ServerPid = pid()
%%		Nickname = string()
%%		Result = {ok, {nick,client()}} | error
get_client( ServerPid, Nickname ) ->
	gen_server:call( ServerPid, {get_client, Nickname} ).

%% @doc
%% retrieve an existing chan from the servernode
%% @end
%% @spec get_chan( ServerPid, ChanName ) -> Result
%% where
%%		ServerPid = pid()
%%		ChanName = string()
%%		Result = {ok, {Channame,pid()}} | error
get_chan( ServerPid, ChanName ) ->
	gen_server:call( ServerPid, {get_chan, ChanName} ).

%% @doc
%%	Add a new chan into the server.
%%	Existance is re-checked to avoid race conditions.
%%	if existance is found, the message is forwarded as
%%	a non-creative join.
%% @end
%% @spec add_chan( ServerPid, Chan ) -> Result
%% where
%%		ServerPid = pid()
%%		Chan = chan()
%%		Result = none
add_chan( ServerPid, Chan ) ->
	gen_server:cast( ServerPid, {add_chan, Chan} ).

%% @doc
%%	Add a new user into the server.
%%	Existance is re-checked,
add_user( ServerPid, User ) ->
	gen_server:call( ServerPid, {add_user, User} ).

%% @doc
%%	Launch a new server.
%% @end
start_link(Supervisor) ->
	gen_server:start_link(?MODULE,
							[#srvs{supervisor=Supervisor,
									clients = ets:new( global_clients, [set] ),
									chans = ets:new( global_chans, [set] ) }],
							[] ).
		
%%
% gen_server implementation
%%
%% @hidden
init( State ) ->
	irc_log:logVerbose( "Server node spawned" ),
	{ok, State}.

is_existing( Table, Key, State ) ->
	case ets:lookup( Table, Key ) of
		[] -> {reply, false, State};
		_ -> {reply, true, State}
	end.

extract( Table, Key, State ) ->
	case ets:lookup( Table, Key ) of
		[] -> {reply, error ,State};
		Obj -> {reply, {ok, Obj}, State}
	end.

%% @hidden
handle_call( {client_exists, Name}, _From, State ) ->
	is_existing( State#srvs.clients, Name, State );

handle_call( {get_client, Nick}, _From, State ) ->
	extract( State#srvs.clients, Nick, State );

handle_call( {chan_exists, Name}, _From, State ) ->
	is_existing( State#srvs.chans, Name, State );
		
handle_call( {get_chan, ChanName}, _From, State ) ->
	extract( State#srvs.chans, ChanName, State );

handle_call( {add_chan, Chan}, _From, State ) ->
	{reply,
        case extract( State#srvs.chans, Chan, State ) of
		    {_,  error , _} -> com_join:server_add( State, Chan );	% faire un join
    		{_, {ok, _}, _} -> State
        end,
     State
	};

%% @hidden
handle_call( _What, _From, State ) ->
	{noreply, State}.	
	
%
% Different call used by the load balancer.
%
%% @hidden
%%%%%%
handle_cast( {set_balance, Clibal, Chanbal},
            #srvs{supervisor=Sup, clients=Cli, chans=Chan,
                    maxcli=Max, maxchanpercli=Maxpercli} ) ->
    {noreply,
        #srvs { supervisor = Sup,
                clients = Cli,
                chanbal = Chanbal,
                clibal = Clibal,
                chans = Chan,
                maxcli = Max,
                maxchanpercli = Maxpercli }
    };
              
handle_cast( _Command, State ) ->
	{noreply, State}.
	
%% @hidden
handle_info(_What, State) ->
	{noreply, State}.

%% @hidden
terminate(_Reason,State) ->
	{ok, State}.

%% @hidden
code_change(_OldVsn, State,_Extra) ->
	{ok, State}.

