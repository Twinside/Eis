-module(chan_manager).

-include("irc_struct.hrl").

-behaviour(gen_server).

% export for the gen_server
-export([init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3]).

-vsn( p01 ).


%%
% gen_server implementation
%%
init(_Args) ->
	undefined.

%
% Different call used by the load balancer.
%
handle_call( {addressource, Chan}, _From, ChanList ) ->
	ets:insert( ChanList, {Chan#chan.channame, Chan} ),
	{noreply, ChanList};

handle_call( {killressource, Chan}, _From, ChanList ) ->
	ets:delete( ChanList, Chan#chan.channame ),
	{noreply, ChanList};
	
handle_call( takeany, _From, ChanList ) ->
	Key = ets:first( ChanList ),
	[Chan] = ets:lookup( ChanList, Key ),
	ets:delete(ChanList, Key),
	{reply, {takeany, Chan}, ChanList}.
	
handle_cast(_Request,_State) ->
	undefined.

handle_info(_Info,_State) ->
	undefined.

terminate(_Reason,_State) ->
	undefined.

code_change(_OldVsn,_State,_Extra) ->
	undefined.

