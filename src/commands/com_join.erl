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
			,server_add/3
            ,get_password/1
		]).

perform_client( Msg, Cli, ClientState ) ->
	Lst = Msg#msg.params,
	NeoMsg = irc:update_sender( Msg, Cli#client.nick ),
	ServerNode = ClientState#listener.servernode,

	case Lst of
		[] -> irc:sendErr( ClientState, Cli, ?ERR_NEEDMOREPARAMS );
        [Dest | _] ->
				(case server_node:get_chan(ServerNode, Dest) of
					error -> create_chan( NeoMsg, Cli, ServerNode );
					{ok, Chan} -> join_chan( NeoMsg, Cli, Chan )
				end)
	end,
	ClientState.


join_chan( Msg, Cli, {Channame,Pid} ) ->
	chan_manager:send_chan( Pid, {Msg, Channame, Cli} )
	.
	
create_chan( Msg, Cli, ServerNode ) ->
	[ChanName|_] = Msg#msg.params,
	server_node:add_chan( ServerNode, ChanName, Cli )
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

% perform_server( _Chan, Serverstate, _IrcMsg ) ->
%	Serverstate.
	
perform_chan( Msg, Cli, Chan, ChanState ) ->
    _ValidJoin = check_password( ChanState, Msg, Chan, Cli ) andalso
                (bnot irc_laws:is_chan_inviteonly( Chan )) andalso
                check_user_limit( ChanState, Cli, Chan ) andalso
                check_user_ban( ChanState, Cli, Chan ),
	ChanState.

%% @doc
%%	Add a chan to the server, internal
%%	process.
%% @end
%% @spec server_add( ServerState, Chan, Owner ) -> none
%% where
%%		ServerState = srvrs()
%%		Chan = string()
%%		Owner = client()
server_add( ServerState, Chan, Owner ) ->
	Bal = ServerState#srvs.chanbal,
	{ok, Pid} = load_balancer:add_ressource( Bal, Chan ),
	ets:add( ServerState#srvs.chans, {Chan, Pid} ),
	gen_server:cast( Pid, {Chan,Owner} )
	.

