-module(client_listener).

-include ("irc_struct.hrl").

-behaviour(gen_server).

% export for the gen_server
-export([init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3]).

%-vsn(prealpha)


%%
% gen_server implementation
%%
init(_Args) ->
	{ok, ets:new()}.

%
% Different call used by the load balancer.
%
handle_call( {addressource, Client}, _From, State ) ->
	ets:insert( State, {Client#client.nick, Client} ),
	{noreply, State};
	
handle_call( {killressource, Client}, _From, State ) ->
	ets:delete( State, Client#client.nick),
	{noreply, State};
	
handle_call( takeany, From, State ) ->
	Key = ets:first( State ),
	[Cli] = ets:lookup( State, Key ),
	ets:delete(State, Key),
	{reply, {takeany, Cli}, State}.

handle_cast(_Request,_State) ->
	undefined.

handle_info(_Info,_State) ->
	undefined.

terminate(_Reason,_State) ->
	undefined.

code_change(_OldVsn,_State,_Extra) ->
	undefined.

