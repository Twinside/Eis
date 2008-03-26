%% @doc
%%	Process in charge of some Channels.
%% @end
-module(chan_manager).

-include("irc_struct.hrl").

-behaviour(gen_server).

% export for the gen_server
-export([start_link/1,
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3]).

-vsn( p01 ).

start_link( Balancer ) ->
	gen_server:start_link( ?MODULE, [Balancer], []).

%%
% gen_server implementation
%%
init( Balance ) ->
	irc_log:logVerbose( "chan manager spawned" ),
	{ok, {Balance ,ets:new(chan_tab, [set])}}.

%% @hidden
handle_call( _Arg, _From, _State ) ->
	undefined.

%
% Different call used by the load balancer.
%
%% @hidden
handle_cast( {addressource, Chan}, ChanList ) ->
	ets:insert( ChanList, {Chan#chan.channame, Chan} ),
	{noreply, ChanList};

%% @hidden
handle_cast( {killressource, Chan}, ChanList ) ->
	ets:delete( ChanList, Chan#chan.channame ),
	{noreply, ChanList};
	
%% @hidden
handle_cast( takeany, ChanList ) ->
	Key = ets:first( ChanList ),
	[Chan] = ets:lookup( ChanList, Key ),
	ets:delete(ChanList, Key),
	{reply, {takeany, Chan}, ChanList};
	
%% @hidden
handle_cast(_Request,_State) ->
	undefined.

%% @hidden
handle_info(_Info,_State) ->
	undefined.

%% @hidden
terminate(_Reason,_State) ->
	irc_log:logVerbose("chan manager terminated" ),
	undefined.

%% @hidden
code_change(_OldVsn,_State,_Extra) ->
	undefined.

