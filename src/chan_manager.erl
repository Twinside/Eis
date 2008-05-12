%% @doc
%%	Process in charge of some Channels.
%% @end
-module(chan_manager).

-include("transaction.hrl").
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
            ,get_user_right/2
            ,broadcast_users/2
            ,broadcast_localusers/2
            ,broadcast_foreignusers/2
            ,broadcast_diff_users/3
            ,broadcast_difflocal/3
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
%%  Get the user rights in a chan.
%% @end
%% @spec get_user_right( Chan, Nick ) -> int()
get_user_right( Chan, Nick ) ->
    case ets:lookup( Chan#chan.userlist, Nick ) of
        [{_,{_, Right}}] -> Right;
        _ -> 0
    end.    

%% @doc
%%  Send message to all members of a chan,
%%  but not to the sender, if he's in.
%% @end
%% @spec broadcast_diff_users( Chan, Message, Nick ) -> none
%% where
%%      Chan = chan()
%%      Message = string()
%%      Nick = string()
broadcast_diff_users( Chan, Message, Nick ) ->
    broadcast_foreignusers( Chan, Message ),
    broadcast_difflocal( Chan, Message, Nick ).
    
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
                    (Usr#client.send)(Usr#client.sendArgs, Msg ),
                    Msg
                   end), 
    ets:foldl( Broadcaster, Message, Chan#chan.userlist )
    .

%% @doc
%%  Send a message to all the local members
%%  of a chan except the user with a specified
%%  nick.
%% @end
%% @spec broadcast_difflocal( Chan , Message, Nick ) -> none
%% where
%%      Chan = chan()
%%      Message = string()
%%      Nick = string()
broadcast_difflocal( Chan, Message, Nick ) ->
    Broadcaster = (fun( {_,{Usr,_Right}}, Msg ) ->
                    Send = Usr#client.nick /= Nick,
                    if Send -> (Usr#client.send)(Usr#client.sendArgs, Msg ),
                               Msg;
                       true -> Msg
                    end
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
        ,max_ban_per_chan = conf_loader:get_int_conf( max_ban_per_chan )
        ,max_ban_length = conf_loader:get_int_conf( max_ban_length )
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
?TRANSACTION_SENTINEL_BEGIN
    Result = dispatch(Msg#msg.ircCommand,
                        Msg,
						Data,
						Chan, State),

	{noreply, Result}
?TRANSACTION_SENTINEL_END( State )
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
dispatch( 'PRIVMSG', Msg, Data, Chan, State ) ->
    com_privmsg:perform_chan( Msg, Data, Chan, State );
dispatch( 'NAMES', Msg, Data, Chan, State ) ->
    com_names:perform_chan( Msg, Data, Chan, State );
dispatch( 'MODE', Msg, Data, Chan, State ) ->
    com_mode:perform_chan( Msg, Data, Chan, State );
dispatch( 'WHO', Msg, Data, Chan, State ) ->
    com_who:perform_chan( Msg, Data, Chan, State );
dispatch( 'QUIT', Msg, Data, Chan, State ) ->
    com_quit:perform_chan( Msg, Data, Chan, State );
dispatch( 'PART', Msg, Data, Chan, State ) ->
    com_part:perform_chan( Msg, Data, Chan, State );
dispatch( 'KICK', Msg, Data, Chan, State ) ->
    com_kick:perform_chan( Msg, Data, Chan, State );
dispatch( 'TOPIC', Msg, Data, Chan, State ) ->
    com_topic:perform_chan( Msg, Data, Chan, State );
dispatch( _, _Msg, _Data, _Chan, State ) ->
	State.
