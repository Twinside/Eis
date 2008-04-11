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
            ,broadcast_users/2
            ,broadcast_localusers/2
            ,broadcast_foreignusers/2
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
	gen_server:cast( Chan, What ).


%% @doc
%%  Send a message to all members of a chan.
%% @end
%% @spec broadcast_users( Chan , Message ) -> none
%% where
%%      Chan = chan()
%%      Message = string()
broadcast_users( Chan, Message ) ->
    broadcast_localusers( Chan, Message ),
    broadcast_foreignusers( Chan, Message ).

%% @doc
%%  Send a message to the users on foreign
%%  server. In reality broadcast the message
%%  to foreign servers which are in charge
%%  of redispatching message at their level.
%% @end
%% @spec broadcast_foreignusers( Chan , Message ) -> none
%% where
%%      Chan = chan()
%%      Message = string()
broadcast_foreignusers( _Chan, _Msg ) -> undefined.

%% @doc
%%  Send a message to all the local members
%%  of a chan.
%% @end
%% @spec broadcast_localusers( Chan , Message ) -> none
%% where
%%      Chan = chan()
%%      Message = string()
broadcast_localusers( Chan, Message ) ->
    Broadcaster = (fun( {_,{Usr,_Right}}, Msg ) ->
                    (Usr#client.send)(Usr#client.sendArgs, Msg )
                   end), 
    ets:foldl( Broadcaster, Message, Chan#chan.userlist )
    .
    
start_link( Initparam ) ->
	gen_server:start_link( ?MODULE, [Initparam], []).

%
% gen_server implementation
%
init( [ {Balance, ServerNode} ] ) ->
	irc_log:logVerbose( "chan manager spawned" ),
	State = #cmanager { bal = Balance,
						serv = ServerNode,
						byname = ets:new(chan_tab, [set]) },
	{ok, reload_config( State )}.

reload_config( State ) ->
	State#cmanager {
		server_host = conf_loader:get_conf( "server_host" )
	}.

%% @hidden
handle_call( _Arg, _From, _State ) ->
	undefined.

%% @doc
%%  Instantiate a new chan.
%% @spec new_chan( Channame ) -> Result
%% where
%%      Channame = string()
%%      Result = chan()
new_chan( Channame ) ->
    #chan {
            channame = Channame,
            userlist = ets:new( gnalist, [set] ),
            foreignusers = ets:new( gneugneu, [set] )
          }.
          
%
% Different call used by the load balancer.
%
%% @hidden
handle_cast( {addressource, Chan}, State ) ->
	ets:insert( State#cmanager.byname, {Chan, new_chan( Chan ) } ),
	{noreply, State};

handle_cast( {killressource, Chan}, ChanList ) ->
	ets:delete( ChanList, Chan#chan.channame ),
	{noreply, ChanList};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast({Msg, ChanName, Data}, State ) ->
    Lst = State#cmanager.byname,
	[{_, Chan}] = ets:lookup( Lst, ChanName ),
	{noreply, dispatch(Msg#msg.ircCommand,
                        Msg,
						Data,
						Chan, State)
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


dispatch( 'JOIN', Msg, Data, Chan, State ) ->
    com_join:perform_chan( Msg, Data, Chan, State );

dispatch( _, _Msg, _Data, _Chan, State ) ->
	State.
