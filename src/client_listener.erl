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
	{ok, {Supervisor, {ets:new(tabtest, [set]), {ets:new(tabtest, [set])}} }.

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
handle_cast( {addressource, Client}, {Super, {UserList, SockList}} ) ->
	ets:insert( UserList, {Client#client.nick, Client} ),
	ets:insert( SockList, {Client#client.sendArgs, Client#client.nick} ),
	{noreply, {Super, {UserList, SockList}}};

%% @hidden	
handle_cast( {killressource, Client}, {Super, {UserList, SockList}} ) ->
	[Cli] = ets:lookup( UserList, Client#client.nick ), % find the client
	% @todo maybe add a log message
	gen_tcp:close( Cli#client.sendArgs ), % close the connection between us
	ets:delete( UserList, Client#client.nick ), % delete his trace from tables
	ets:delete( SockList, Socket ),
	{noreply, {Super, UserList}};
	
handle_cast( takeany, {Super, {UserList, SockList}} ) ->
	Key = ets:first( UserList ),
	[Cli] = ets:lookup( UserList, Key ),
	% we give the responsabilitie of the socket to the supervisor
	gen_tcp:controlling_process( Cli#client.sendArgs, Super ),
	ets:delete( SockList, Cli#client.sendArgs ), % deleting from tables
	ets:delete( UserList, Cli#client.nick ),
	{reply, {takeany, Cli}, {Super, {UserList, SockList}}};

handle_cast( Msg, {Super, {UserList, SockList}} ) when is_record( Msg, msg ) ->
	% do not convert Msg to StrMsg before know if the client is virtual or not
	ets:foldl(broadcaster, Msg, UserTable),
	{noreply, {Super, {UserList, SockList}};

handle_cast( _Request, State ) -> % ignore invalid cast
	{noreply, State}.
	
%% @hidden
handle_info( {tcp, Socket, Data}, {Super, {UserList, SockList}} ) ->
	Msg = irc:msg_of_string( Data ),
	Cli = ets:lookup( UserList, ets:lookup( SockList, Socket ) ),
	% do right things with Cli and Msg
	io:format( "TCP message received from ~p~nMassage is : ~p~n", [Cli, MSG] ),
	{noreply, State}.

%% @hidden
terminate(_Reason,_State) ->
	irc_log:logVerbose( "Client Listener terminated" ),
	undefined.

%% @hidden
code_change(_OldVsn,_State,_Extra) ->
	undefined.

