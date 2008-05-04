-module( com_quit ).

-include( "transaction.hrl" ).
-include( "irc_struct.hrl" ).

-vsn( p01 ).

-export([
             perform_client/3
            ,perform_chan/4
            ,client_cleanup/3
        ]).

perform_client( Msg, Cli, ClientState ) ->
    NeoMsg = irc:update_sender( Msg, Cli ),
    server_node:del_user( ClientState#listener.servernode, Cli#client.nick ),
    {_, Sock} = Cli#client.sendArgs,
    gen_tcp:close( Sock ),
    client_cleanup( NeoMsg, Cli, ClientState )
    .
    
perform_chan( _Msg, {MsgTosend, User}, Chan, ChanState ) ->
    case com_part:cleanup_chan( Chan, User, ChanState ) of
        {removed, NeoState} -> NeoState;
        {ok, State} ->
           [{_,NeoChan}] = ets:lookup( State#cmanager.byname, Chan#chan.channame ),
           chan_manager:broadcast_users( NeoChan, MsgTosend ),
           State
    end
    .

send_quit( Cli, Msg ) ->
    Tosend = irc:string_of_msg( Msg ),
    Dispatcher = (fun( {Chan,Pid}, _ ) ->
                    chan_manager:send_chan( Pid, {Msg, Chan, {Tosend, Cli}} ),
                    0
                   end),
    lists:foldl( Dispatcher, 0, Cli#client.is_in )
    .

%% @doc
%%  This function is in charge of removing
%%  a client from the server in all places.
%%  The msg in parameter is the quit message
%%  to broad_cast to chan users.
%% @end
%% @spec client_cleanup( Msg, Cli, State ) -> State
%% where
%%      Msg = msg()
%%      Cli = client()
%%      State = listener()
client_cleanup( Msg, Cli, State ) ->
?TRANSACTIONBEGIN
    {_, Socket} = Cli#client.sendArgs,
    Nick = Cli#client.nick,
    ets:delete( State#listener.bysock, Socket ),
    ets:delete( State#listener.bynick, Nick ),
    server_node:del_user( State#listener.servernode, Nick ),
    load_balancer:notif_killed( State#listener.supervisor )
?TRANSACTIONEND,
    irc_log:logEvent( "Client disconected : " ++ irc:cli_to_string( Cli ) ),
    send_quit( Cli, Msg ),
    State
    .

