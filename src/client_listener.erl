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

start_link( Balancer ) ->
	gen_server:start_link( ?MODULE, [Balancer], [] ).


%%
% gen_server implementation
%%
init( Supervisor ) ->
	{ok, {Supervisor, ets:new(tabtest, [set])} }.

handle_call( _What, _From, _State ) ->
	undefined.
	

% for casting irc messages
broadcaster( User, StrMsg ) ->
	(User#client.send)( User, StrMsg ),
	StrMsg.
	
%
% Different call used by the load balancer.
%
handle_cast( {addressource, Client}, {Super, UserList} ) ->
	ets:insert( UserList, {Client#client.nick, Client} ),
	{noreply, {Super, UserList}};
	
handle_cast( {killressource, Client}, {Super, UserList} ) ->
	ets:delete( UserList, Client#client.nick),
	{noreply, {Super, UserList}};
	
handle_cast( takeany, {Super, UserList} ) ->
	Key = ets:first( UserList ),
	[Cli] = ets:lookup( UserList, Key ),
	ets:delete(UserList, Key),
	{reply, {takeany, Cli}, {Super, UserList}};

handle_cast( Msg, {Super,UserTable} ) when is_record(Msg, msg) ->
	StrMsg = irc:string_of_msg( Msg ),
	ets:foldl(broadcaster, StrMsg, UserTable),
	{noreply, {Super,UserTable}};

handle_cast( _Request, State ) -> % ignore invalid cast
	{noreply, State}.
	
handle_info({tcp, _Socket, Data}, State) ->
	msg = irc:msg_of_string( Data ),
	{noreply, State}.

terminate(_Reason,_State) ->
	undefined.

code_change(_OldVsn,_State,_Extra) ->
	undefined.

