%% @doc
%%	Module taking in charge all the code related
%%	to the irc JOIN command.
%% @end
-module( com_join ).

-include( "irc_struct.hrl" ).

-vsn( p01 ).

-export([
			perform_client/3
			,perform_chan/4
			,server_add/2
            ,get_password/1
		]).

%% @doc
%%  Process the client side of the JOIN command.
%%  It's called by the client_listener.
%% @end
%% @spec perform_client( Msg, Cli, ClientState ) -> Result
%% where
%%      Msg = msg()
%%      Cli = client()
%%      ClientState = listener()
%%      Result = listener()
perform_client( Msg, Cli, ClientState ) ->
	Lst = Msg#msg.params,
	NeoMsg = irc:update_sender( Msg, Cli#client.nick ),

	case Lst of
		[] -> irc:sendErr( ClientState, Cli, ?ERR_NEEDMOREPARAMS );
        [Dest | _] -> do_client_join( Dest, NeoMsg, Cli, ClientState )
	end,
	ClientState.

%% @doc
%%  Validate a channel name and send the
%%  apropriate error message if there is an error.
%% @end
%% @spec name_validation( ClientState, Cli, Channame ) -> bool
%% where
%%      ClientState = listener()
%%      Cli = client()
%%      Channame = string()
name_validation( ClientState, Cli, Channame ) ->
    Valid = irc:is_channame_valid( Channame ),
    if Valid -> true;
        true -> Msg = ?ERR_NOSUCHCHANNEL ++ Channame ++ ?ERR_NOSUCHCHANNEL_TXT, 
                irc:send_err( ClientState, Cli, Msg ),
                false
    end.

do_client_join( Dest, Msg, Cli, CliState ) ->
    Continue = name_validation( CliState, Cli, Dest ),
	ServerNode = CliState#listener.servernode,
    if Continue -> case server_node:get_chan(ServerNode, Dest) of
                    error -> create_chan( Msg, Cli, ServerNode );
                    {ok, Chan} -> join_chan( Msg, Cli, {Dest, Chan} )
                end;

       true -> false
    end.

     
join_chan( Msg, Cli, {Channame,Pid} ) ->
	chan_manager:send_chan( Pid, {Msg, Channame, Cli} )
	.

	
create_chan( Msg, Cli, ServerNode ) ->
	[ChanName|_] = Msg#msg.params,
	Pid = server_node:add_chan( ServerNode, ChanName ),
    chan_manager:send_chan( Pid, {Msg, ChanName, Cli} )
	.

%% @doc
%%  Extract the password of a JOIN message
%% @end
%% @spec get_password( Msg ) -> Result
%% where
%%      Msg = msg()
%%      Result = string()
get_password( Msg ) ->
    case Msg#msg.params of
        [_, Password | _] -> Password;
        _ -> ""
    end.

%% @doc
%%  Check if the chan is passworded and compare
%%  given passwords.
%% @end
%% @spec check_password( State, Msg, Chan, Cli ) -> bool
%% where
%%      State = cmanager()
%%      Msg = msg()
%%      Chan = chan()
%%      Cli = client()
check_password( State, Msg, Chan, Cli ) ->
    Passworded = irc_laws:is_chan_passworded(Chan),
    if Passworded -> 
            Pass = get_password( Msg ),
            if Pass /= Chan#chan.password ->
                    irc:send_err( State, Cli, ?ERR_BADCHANNELKEY );
                    false;
               true -> true
            end;
        true -> true
    end.

%% @spec check_user_limit( State, Chan, Cli ) -> bool
%% where
%%      State = cmanager()
%%      Chan = chan()
%%      Cli = client()
check_user_limit( State, Cli, Chan ) ->
    Limited = irc_laws:is_chan_limited( Chan ),
    if  Limited andalso
        Chan#chan.userlimit =< Chan#chan.usercount + 1 -> 
                irc:send_err(State, Cli, ?ERR_CHANNELISFULL),
                false;

        true -> true
    end.

%% @spec check_user_ban( State, Chan, Cli ) -> bool
%% where
%%      State = cmanager()
%%      Chan = chan()
%%      Cli = client()
check_user_ban( _State, _Cli, _Chan ) ->
    true.

%% @doc
%%  Perform validation and registering of the JOIN
%%	command on the cannel side. Called by chan_manager.
%% @end
%% @spec perform_chan( Msg, Cli, Chan, ChanState ) -> Result
%% where
%%      Msg = msg()
%%      Cli = client()
%%      Chan = channel()
%%      ChanState = cmanager()
perform_chan( Msg, Cli, Chan, ChanState ) ->
    ValidJoin = check_password( ChanState, Msg, Chan, Cli ) andalso
                (not irc_laws:is_chan_inviteonly( Chan )) andalso
                check_user_limit( ChanState, Cli, Chan ) andalso
                check_user_ban( ChanState, Cli, Chan ),
                
    if ValidJoin -> register_user( Cli, ChanState, Chan,
                                    server_node:is_cli_local( Cli ) );
        true -> ChanState
    end
	.
%
% when a foreign connect
%
register_user( Cli, ChanState, Chan, false ) ->
    Right = irc_laws:choose_welcome_right( Chan#chan.usercount ),
    NeoChan = Chan#chan {usercount = Chan#chan.usercount + 1 },
    ets:insert( NeoChan#chan.foreignusers,
                {Cli#client.nick, Right} ),
    % update the chan information.
    ets:insert( ChanState#cmanager.byname,
                {Chan#chan.channame, NeoChan} ),
    Notif = irc:forge_msg(Cli,'JOIN',[], Chan#chan.channame),
    chan_manager:broadcast_localusers( Chan, Notif ),
    ChanState;

% when a local connect.
register_user( Cli, ChanState, Chan, true ) ->
    Right = irc_laws:choose_welcome_right( Chan#chan.usercount ),
    NeoChan = Chan#chan {usercount = Chan#chan.usercount + 1 },
    ets:insert( NeoChan#chan.userlist,
                {
                    Cli#client.nick,
                    {Cli, Right}
                } ),
    % update the chan information.
    ets:insert( ChanState#cmanager.byname,
                {NeoChan#chan.channame, NeoChan} ),
    send_welcome_info( ChanState#cmanager.server_host,
                        Cli, Chan ),
    Notif = irc:forge_msg(Cli,'JOIN',[], Chan#chan.channame),
    chan_manager:broadcast_localusers( Chan, Notif ),
    % filer la liste des noms aussi, �a peut aider.     
    ChanState
    .
    
send_welcome_info( Serverhost, Cli, #chan {channame = ChanName,
                                            topic = Topic } ) ->
    Message = irc:forge_msg( Serverhost, ?RPL_TOPIC, [ChanName], Topic ),
    (Cli#client.send)( Cli#client.sendArgs, Message ).
                                            
    
%% @doc
%%	Add a chan to the server, internal
%%	process.
%% @end
%% @spec server_add( ServerState, Channame ) -> Result
%% where
%%		ServerState = srvrs()
%%		Channame = string()
%%      Result = srvs()
server_add( ServerState, Channame ) ->
	Bal = ServerState#srvs.chanbal,
	Pid = load_balancer:add_ressource( Bal, Channame ),
	ets:insert( ServerState#srvs.chans, {Channame, Pid} ),
    {Pid, ServerState}
	.

