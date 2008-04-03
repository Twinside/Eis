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
%% where Balancer = pid()
%%		 Result = {ok, Pid} | {error| Error}
start_link( Balancer ) ->
	gen_server:start_link( ?MODULE, [Balancer], [] ).


%%
% gen_server implementation
%%
%% @hidden
init( Supervisor ) ->
	irc_log:logVerbose( "Client Listener created" ),
	{ok, #listener{ supervisor = Supervisor,
					bynick = ets:new(tabtest, [set]),
					bysock = ets:new(tabtest, [set]) }
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
	ets:insert( State#listener.bynick, {Client#client.nick, Client} ),
	ets:insert( State#listener.bysock, {Client#client.sendArgs, Client#client.nick} ),
	{noreply, State};

%% @hidden	
handle_cast( {killressource, Client}, State ) ->
	Bynick = State#listener.bynick,
	Bysock = State#listener.bysock,
	case ets:lookup( Bynick, Client#client.nick ) of
		[] -> {noreply, State};
		[Cli] -> gen_tcp:close( Cli#client.sendArgs ), % close the connection between us
				ets:delete( Bynick, Client#client.nick ), % delete his trace from tables
				ets:delete( Bysock, Client#client.sendArgs ),
				{noreply, State}
	end;

handle_cast( takeany, State ) ->
	Bynick = State#listener.bynick,
	Bysock = State#listener.bysock,
	
	Key = ets:first( Bynick ),
	[Cli] = ets:lookup( Bynick, Key ),
	ets:delete( Bynick, Cli#client.sendArgs ), % deleting from tables
	ets:delete( Bysock, Cli#client.nick ),
	
	% we give the responsabilitie of the socket to the supervisor
	gen_tcp:controlling_process( Cli#client.sendArgs,
								 State#listener.supervisor ),
	{reply, {takeany, Cli}, State};

handle_cast( Msg, State ) when is_record( Msg, msg ) ->
	% do not convert Msg to StrMsg before know if the client is virtual or not
	% ets:foldl(broadcaster, Msg, UserTable),
	{noreply, State};

handle_cast( _Request, State ) -> % ignore invalid cast
	{noreply, State}.
	
%% @hidden
handle_info( {tcp, Socket, Data}, State ) ->
	Bysock = State#listener.bysock,
	Msg = irc:msg_of_string( Data ),
	[Cli] = ets:lookup( Bysock, Socket ),
	{noreply, dispatcher( Msg#msg.ircCommand, Msg, Cli, State ) }.

%% @hidden
terminate(_Reason,_State) ->
	irc_log:logVerbose( "Client Listener terminated" ),
	undefined.

%% @hidden
code_change(_OldVsn,_State,_Extra) ->
	undefined.


dispatcher( 'JOIN', Msg, From, State ) ->
	com_join:perform_client( Msg, From, State ).
%dispatcher( 'PRIVMSG', Msg, From ) -> command_privmsg( Msg );
%dispatcher( 'NOTICE', Msg, From ) -> command_notice( Msg ).

% command_privmsg( Msg ) -> true.  
% command_notice( Msg ) -> true.  
%command_join( Msg ) -> true.
