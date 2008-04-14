-module( com_quit ).

-include( "irc_struct.hrl" ).

-vsn( p01 ).

-export([
            perform_client/3
            ,perform_chan/4
        ]).

perform_client( Msg, Cli, ClientState ) ->
    Tosend = prepare_sended_text( Msg, Cli ),
    Dispatcher = (fun( {Chan,Pid}, _ ) ->
                    chan_manager:send_chan( Pid, {Msg, Chan, {Tosend, Cli}} )
                   end),
    lists:foldl( Dispatcher, 0, Cli#client.is_in ),
    server_node:del_user( ClientState#listener.servernode, Cli#client.nick ),
    {_, Sock} = Cli#client.sendArgs,
    gen_tcp:close( Sock ),
    ClientState.

prepare_sended_text( Msg, Cli ) ->
    NeoMessage = irc:update_sender( Msg, Cli ),
    irc:string_of_msg( NeoMessage ).
    
perform_chan( _Msg, {MsgTosend, User}, Chan, ChanState ) ->
    case com_part:cleanup_chan( Chan, User, ChanState ) of
        {removed, NeoState} -> NeoState;
        {ok, State} ->
           [{_,NeoChan}] = ets:lookup( State#cmanager.byname, Chan#chan.channame ),
           chan_manager:broadcast_users( NeoChan, MsgTosend ),
           State
    end.

