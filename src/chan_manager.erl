%% @doc
%%	Process in charge of some Channels.
%% @end
-module(chan_manager).

-include("irc_struct.hrl").

-behaviour(gen_server).

% export for the gen_server
-export([
			start_link/1,
			init/1,
			handle_call/3,
			handle_cast/2,
			handle_info/2,
			terminate/2,
			code_change/3
		]).

-export([
			send_chan/2
		]).
		
-vsn( p01 ).

%% @doc
%%	Send a message to the manager of a chan.
%% @end
%% @spec send_chan( Chan, What ) -> Result
%% where
%%		Chan = {pid(), string()}
%%		What = term()
%%		Result = ok
send_chan( Chan, What ) ->
	gen_server:handle_cast( Chan#chan.manager,
							What ).

start_link( Balancer ) ->
	gen_server:start_link( ?MODULE, [Balancer], []).

%
% gen_server implementation
%
init( {Balance, ServerNode} ) ->
	irc_log:logVerbose( "chan manager spawned" ),
	State = #cmanager { bal = Balance,
						serv = ServerNode,
						byname = ets:new(chan_tab, [set]) },
	{ok, State}.

%% @hidden
handle_call( _Arg, _From, _State ) ->
	undefined.

%
% Different call used by the load balancer.
%
%% @hidden
handle_cast( {addressource, Chan}, State ) ->
	ets:insert( State#cmanager.byname,
				{Chan#chan.channame, Chan} ),
	{noreply, State};

%% @hidden
handle_cast( {killressource, Chan}, ChanList ) ->
	ets:delete( ChanList, Chan#chan.channame ),
	{noreply, ChanList};
	
%% @hidden
handle_cast( takeany, ChanList ) ->
	Key = ets:first( ChanList ),
	[{_, Chan}] = ets:lookup( ChanList, Key ),
	ets:delete(ChanList, Key),
	{reply, {takeany, Chan}, ChanList};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast({Msg, Data}, State ) ->
	Chan = 
	{noreply, dispatch(Msg#msg.ircCommand,
						Data,
						State)
	}
	;
	
%% @hidden
handle_cast(_Request,_State) -> undefined.

%% @hidden
handle_info(_Info,_State) -> undefined.

%% @hidden
terminate(_Reason,_State) ->
	irc_log:logVerbose("chan manager terminated" ),
	undefined.

%% @hidden
code_change(_OldVsn,_State,_Extra) -> undefined.


dispatch( 'JOIN', Data, State ) -> com_join:perform_chan( Data, State );

dispatch( _, _Data, State ) ->
	State.
