%% @doc
%%	Module containing all the code related to
%%	the IRC part command.
%% @end
-module( com_part ).

-include( "transaction.hrl" ).
-include( "irc_struct.hrl" ).

-vsn( p01 ).

-export([
            perform_client/3
            ,perform_chan/4
            ,cleanup_chan/3
        ]).
%% @doc
%%  Remove an user from a chan. The chan is automatically
%%  deleted if the chan is empty.
%% @end
%% @spec cleanup_chan( Chan, User, ChanState ) -> Cleaned
%% where
%%      Chan = chan()
%%      User = client()
%%      ChanState = cmanager()
%%      Cleaned = {ok, State} | {removed, State}
cleanup_chan( Chan, User, ChanState ) ->
    Local = server_node:is_cli_local( User ),
    user_remover( Chan, User, Local ),
    NeoChan = Chan#chan {usercount = Chan#chan.usercount - 1 },
?TRANSACTIONBEGIN
    % we remove the chan if it's empty.
    if NeoChan#chan.usercount == 0 ->
               server_node:del_chan( ChanState#cmanager.serv,
                                     NeoChan#chan.channame ),
               ets:delete( NeoChan#chan.userlist ),
               ets:delete( NeoChan#chan.foreignusers ),
               ets:delete( ChanState#cmanager.byname, NeoChan#chan.channame ),
               load_balancer:notif_killed( ChanState#cmanager.bal ),
               {removed, ChanState};

       true -> ets:insert( ChanState#cmanager.byname,
                            {NeoChan#chan.channame, NeoChan} ),
               {ok, ChanState}
    end
?TRANSACTIONEND
    .
    
user_remover( Chan, User, true ) -> % local
    ets:delete( Chan#chan.userlist, User#client.nick );
user_remover( Chan, User, _ ) -> % foreign
    ets:delete( Chan#chan.foreignusers, User#client.nick )
    .

% Not enough parameters
perform_client( #msg { params=[] }, Cli, ClientState ) ->
    irc:send_err( ClientState, Cli, ?ERR_NEEDMOREPARAMS );

perform_client( Msg, Cli, ClientState ) ->
    UMsg = irc:update_sender( Msg, Cli ),
    Iterer = (fun( ChanName ) ->
                NeoMsg = UMsg#msg{ params = [ChanName] },
                case [Pid || {Name, Pid} <- Cli#client.is_in, Name == ChanName] of
                    [PPid] -> chan_manager:send_chan( PPid, {NeoMsg, ChanName, Cli} ),
                              Filtered = [{N, P} || {N,P} <- Cli#client.is_in, N /= ChanName],
                              NeoCli = Cli#client{ is_in = Filtered },
                           ?TRANSACTIONBEGIN
                              ets:insert( ClientState#listener.bynick, {NeoCli#client.nick, NeoCli} )
                           ?TRANSACTIONEND;

                    [] -> Errmsg = ?ERR_NOTONCHANNEL
                                 ++ Cli#client.nick
                                 ++ [$  | ChanName ]
                                 ++ [$  | ?ERR_NOTONCHANNEL_TXT],
                          irc:send_err( ClientState, Cli, Errmsg )
                end
             end),
    lists:foreach( Iterer, Msg#msg.params ),
    ClientState.

perform_chan( Msg, Cli, Chan, ChanState ) ->
    StrMsg = irc:string_of_msg( Msg ),
    chan_manager:broadcast_users( Chan, StrMsg ),
    {_, State} = cleanup_chan( Chan, Cli, ChanState ),
    State
    .

