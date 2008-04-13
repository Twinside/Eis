-module(client_listener).

-include ("irc_struct.hrl").

-behaviour(gen_server).

% export for the gen_server
-export([init/1,
		start_link/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3]).

% other export
-export( [broadcaster/2] ).

-vsn( p01 ).
%% @doc
%%	Start e new client listener.
%% @end
%% @spec start_link( Balancer ) -> Result
%% where InitParam = {pid(), pid()}
%%		 Result = {ok, Pid} | {error, Error}
start_link( Initparam ) ->
	gen_server:start_link( ?MODULE, [Initparam], [] ).


%%
% gen_server implementation
%%
%% @hidden

init( [ {Balance, Servernode} ] ) ->
	irc_log:logVerbose( "Client Listener created" ),
	State = #listener{ supervisor = Balance
                    	,servernode = Servernode
						,bynick = ets:new(tabtest, [set])
						,bysock = ets:new(tabtest, [set]) },
	{ok, reload_config( State ) }.

reload_config( State ) ->
	State#listener{
		server_host = conf_loader:get_conf( "server_host" )
		,maxchanpercli = conf_loader:get_int_conf( "chan_per_cli" )
	}.

%% @hidden
handle_call( _What, _From, _State ) ->
	undefined.
	

% for casting irc messages
broadcaster( User, Msg ) ->
	(User#client.send)( User, Msg ),
	Msg.

%
% Different call used by the load balancer.
%
%% @hidden
handle_cast( {addressource, Client}, State ) ->
    {_, Sock} = Client#client.sendArgs,
    Neocli = Client#client{ cli_listener = self() },
	ets:insert( State#listener.bynick, {Client#client.nick, Neocli} ),
	ets:insert( State#listener.bysock, {Sock, Client#client.nick} ),
	{noreply, State};

%% @hidden	
handle_cast( {killressource, Client}, State ) ->
	Bynick = State#listener.bynick,
	case ets:lookup( Bynick, Client#client.nick ) of
		[] -> {noreply, State};
		[Cli] -> {_, Sock} = Cli#client.sendArgs,
                gen_tcp:close( Sock ), % close the connection between us
                % rest of the cleaning is made during
                % the socket cleanup
				{noreply, State}
	end;

handle_cast( {notifjoin, Cliname, Chaninfo}, State ) ->
    [{_,Cli}] = ets:lookup( State#listener.bynick, Cliname ),
    Neocli = Cli#client{ is_in = [Chaninfo|Cli#client.is_in] },
    ets:insert( State#listener.bynick, {Cliname,Neocli} ),
    {noreply, State};
                                        
%% @doc
%%  Clause made uniquely to inject
%%  data for unit testing.
%% @end
handle_cast( {test, Nick, Data}, State ) ->
    Bynick = State#listener.bynick,
	Msg = irc:msg_of_string( Data ),
    [{_,Cli}] = ets:lookup( Bynick, Nick ),
	{noreply, dispatcher( Msg#msg.ircCommand, Msg, Cli, State ) };
    
handle_cast( Msg, State ) when is_record( Msg, msg ) ->
	% do not convert Msg to StrMsg before know if the client is virtual or not
	% ets:foldl(broadcaster, Msg, UserTable),
	{noreply, State};

handle_cast( _Request, State ) -> % ignore invalid cast
	{noreply, State}.

	
%% @hidden
handle_info( {tcp, Socket, Data}, State ) ->
	Bysock = State#listener.bysock,
    Bynick = State#listener.bynick,
	Msg = irc:msg_of_string( Data ),
	[{_, Nick}] = ets:lookup( Bysock, Socket ),
    [{_,Cli}] = ets:lookup( Bynick, Nick ),
	{noreply, dispatcher( Msg#msg.ircCommand, Msg, Cli, State ) };

handle_info( {tcp_closed, Socket} , State ) ->
	[{_, Nick}] = ets:lookup( State#listener.bysock, Socket ),
    [{_,Cli}] = ets:lookup( State#listener.bynick, Nick ),
    ets:delete( State#listener.bysock, Socket ),
    ets:delete( State#listener.bynick, Nick ),
    server_node:del_user( State#listener.servernode, Nick ),
    load_balancer:notif_killed( State#listener.supervisor ),
    irc_log:logEvent( "Client deconnexion : " ++ Cli#client.nick ),
    % TODO : propagate deconnexion message to everyone.
    {noreply, State}
    .
    
%% @hidden
terminate(Reason, State) ->
	irc_log:logVerbose( "Client Listener terminated" ),
    ets:delete( State#listener.bysock ),
    ets:delete( State#listener.bynick ),
    Reason.

%% @hidden
code_change(_OldVsn,_State,_Extra) ->
	undefined.


dispatcher( 'JOIN', Msg, From, State ) ->
	com_join:perform_client( Msg, From, State );
dispatcher( 'NOTICE', Msg, From, State ) ->
	com_notice:perform_client( Msg, From, State );
dispatcher( 'PRIVMSG', Msg, From, State ) ->
    com_privmsg:perform_client( Msg, From, State );
dispatcher( 'NAMES', Msg, From, State ) ->
    com_names:perform_client( Msg, From, State );
dispatcher( 'WHO', Msg, From, State ) ->
    com_who:perform_client( Msg, From, State );
dispatcher( 'QUIT', Msg, From, State ) ->
    com_quit:perform_client( Msg, From, State );

dispatcher( {unknown_command, Txt}, _Msg, From, State ) ->
    Notice = irc:forge_msg( State#listener.server_host, ?ERR_UNKNOWNCOMMAND
                            ,[Txt]
                            ,?ERR_UNKNWONCOMMAND_TXT ++ Txt ),
    (From#client.send)(From#client.sendArgs, Notice ),
    State; 
dispatcher( Command, Msg, From, State ) ->
    dispatcher( {unknown_command, atom_to_list( Command )}, Msg, From, State )
    .

