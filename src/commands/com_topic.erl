-module( com_topic ).

-include( "irc_struct.hrl" ).
-vsn( p01 ).

-export([
            perform_client/3,
            perform_chan/4
		]).
		
perform_client( #msg { params=[] }, Cli, ClientState ) ->
    irc:send_err( ClientState, Cli, ?ERR_NEEDMOREPARAMS );
		
perform_client( Msg, Cli, ClientState ) ->
    UMsg = irc:update_sender( Msg, Cli ),
	[ Chan | _ ]  = Msg#msg.params,
    case [Pid || {Name,Pid} <- Cli#client.is_in, Name == Chan] of
    
        [CPid] -> chan_manager:send_chan( CPid, {UMsg, Chan, Cli} ),
                  ClientState;
                  
        _ -> Errmsg = ?ERR_NOSUCHCHANNEL
                     ++ Cli#client.nick
                     ++ [$  | Chan ]
                     ++ [$  | ?ERR_NOSUCHCHANNEL_TXT],
                     irc:send_err( ClientState, Cli, Errmsg ),
                     ClientState
    end.

perform_chan( Msg, Cli, Chan, ChanState ) ->
    URight = chan_manager:get_user_right( Chan, Cli#client.nick ),
    CMode = Chan#chan.mode,
    Valid = irc_laws:check_chanlaw( 'TOPIC' , URight, CMode ),

    if Valid -> NeoChan = Chan#chan { topic = Msg#msg.data },
                ets:insert( ChanState#cmanager.byname,
                            {NeoChan#chan.channame, NeoChan} ),
                StrMsg = irc:string_of_msg( Msg ),
                chan_manager:broadcast_users( Chan, StrMsg ),
                ChanState;
       
       true -> Errmsg = ?ERR_CHANOPPRIVSNEEDED
                     ++ [$  | Chan#chan.channame ]
                     ++ [$  | ?ERR_CHANOPPRIVSNEEDED_TXT],
                     irc:send_err( ChanState, Cli, Errmsg ),
                     ChanState
 
    end.
	
