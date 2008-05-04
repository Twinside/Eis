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
    
perform_client( _Msg, _Cli, ClientState ) ->
    ClientState.

perform_chan( _Msg, _Data, _Chan, ChanState ) ->
    ChanState.

