%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% <p>The server_node module is a "gen_server" which store all
%%	all the global informations required to be known across the
%%	server.</p>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module( server_node ).

-behaviour( gen_server ).

-include_lib("kernel/include/inet.hrl").
-include( "irc_struct.hrl" ).


-export([
			is_client_existing/2
			,is_chan_existing/2
			,get_client/2
			,get_chan/2
			,add_chan/2
			,add_user/2
            ,is_cli_local/1
            ,is_cli_foreign/1
            ,is_cli_virtual/1
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

-export( [allowed/0 ]).
allowed( ) -> [ 'QUIT', 'NOTICE', 'PRIVMSG', 'KICK', 'MODE', 'WHO', 'WHOIS', 'WHOWAS' , 'NAMES'  ].

%% @doc
%%  Tell if a client is a local one registered
%%  here.
%% @end
%% @spec is_cli_local( Client ) -> bool
%% where
%%      Client = client()
is_cli_local( #client{ sendArgs={local,_ }} ) -> true;
is_cli_local( _ ) -> false.

%% @doc
%%  Tell if a client is connected to a remote
%%  server.
%% @end
%% @spec is_cli_foreign( Client ) -> bool
%% where
%%      Client = client()
is_cli_foreign( #client{ sendArgs={foreign, _}} ) -> true;
is_cli_foreign( _ ) -> false.

%% @doc
%%  Tell if a client is a virtual one. IE a virtual client
%%  normaly run a service, can be a bot by exemple.
%% @end
%% @spec is_cli_virtual( Client ) -> bool
%% where
%%      Client = client()
is_cli_virtual( #client{ sendArgs = {virtual, _}} ) -> true;
is_cli_virtual( _ ) -> false.

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
	gen_server:call( ServerPid, {add_chan, Chan} ).

%% @doc
%%	Add a new user into the server.
%%	Existance is re-checked,
%% @end
%% @spec add_user( ServerPid, User ) -> Result
%% where
%%      ServerPid = pid()
%%      User = client()
%%      Result = ok | error
add_user( ServerPid, User ) ->
    user_adding( ServerPid, User, is_cli_local( User ) ).

user_adding( ServerPid, User, true ) ->
    {_, Sock} = User#client.sendArgs,
    ok = gen_tcp:controlling_process( Sock, ServerPid ),
	gen_server:call( ServerPid, {add_user_local, User} );
user_adding( _ServerPid, _User, false ) ->
    ok. % TODO for foreign & virtual users

%% @doc
%%	Launch a new server.
%% @end
start_link(Supervisor) ->
	gen_server:start_link(?MODULE, [ Supervisor ], [] ).


%%
% gen_server implementation
%%
%% @hidden
init( [Supervisor] ) ->
	irc_log:logVerbose( "Server node spawned" ),
    NeoState = #srvs{ clients = ets:new( global_clients, [set] )
                        ,chans = ets:new( global_chans, [set] )
                        ,foreignscli = ets:new( global_foreign, [set])
                        ,supervisor = Supervisor },
	{ok,  reload_config( NeoState ) }.

reload_config( State ) ->
	State#srvs{
				maxcli = conf_loader:get_int_conf( "server_max_client" )
				,maxchan = conf_loader:get_int_conf( "server_max_chan" )
				,maxchanpercli = conf_loader:get_int_conf( "chan_per_cli" )
			  }.

is_existing( Table, Key, State ) ->
	case ets:lookup( Table, Key ) of
		[] -> {reply, false, State};
		_ -> {reply, true, State}
	end.

extract( Table, Key, State ) ->
	case ets:lookup( Table, Key ) of
		[] -> {reply, error ,State};
		[{_, Obj}] -> {reply, {ok, Obj}, State}
	end.

%% @hidden
handle_call( {client_exists, Name}, _From, State ) ->
	case is_existing( State#srvs.clients, Name, State ) of
        {_, false, _} -> is_existing( State#srvs.foreignscli, Name, State );
        Else -> Else
    end;

handle_call( {get_client, Nick}, _From, State ) ->
	case extract( State#srvs.clients, Nick, State ) of
        {_, error, _} -> extract( State#srvs.foreignscli, Nick, State );
        Else -> Else
    end;


handle_call( {add_chan, Chan}, _From, State ) ->
    case extract( State#srvs.chans, Chan, State ) of
        {_,  error , _} -> {Pid, St} = com_join:server_add( State, Chan ),
                            {reply, Pid, St};
        {_, {ok, Pid}, _} -> {reply, Pid, State}
    end;
    
handle_call( {chan_exists, Name}, _From, State ) ->
	is_existing( State#srvs.chans, Name, State );
		
handle_call( {get_chan, ChanName}, _From, State ) ->
	extract( State#srvs.chans, ChanName, State );

handle_call( {add_user_local, User}, _From, State ) ->
    Exist = handle_call( {client_exists, User#client.nick}, 0, State ),
    if Exist -> {reply, {error, "Nick already in use"}, State};
       true ->
        {_, Sock} = User#client.sendArgs,
        Pid = load_balancer:add_ressource( State#srvs.clibal, User ),
        gen_tcp:controlling_process( Sock, Pid ),
	    inet:setopts(Sock, [{active, true}]),
        ets:insert( State#srvs.clients, {User#client.nick, User} ),
        {reply, ok, State}
    end;

%% @hidden
handle_call( _What, _From, State ) ->
	{noreply, State}.	
	
%
% Different call used by the load balancer.
%
%% @hidden
%%%%%%
handle_cast( {set_balance, Clibal, Chanbal}, State) ->
    NewState = State#srvs {
                    chanbal = Chanbal,
                    clibal = Clibal},
    {noreply,NewState};
              
    
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

