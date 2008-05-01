%% @doc
%%  All the code related to the IRC
%%  WHO command.
%% @end
-module( com_who ).

-include( "irc_struct.hrl" ).

-vsn( p01 ).


-export([
            perform_client/3
            ,perform_chan/4
        ]).

perform_client( Msg, Cli, ClientState ) ->
    case Msg#msg.params of
        [_Chan|_] -> analyse( Msg, Cli, ClientState );
        _ -> ClientState
    end.

analyse( Msg, Cli, ClientState ) ->
    [Chan|_] = Msg#msg.params,
    Remain = [Pid || {Name,Pid} <- Cli#client.is_in, Name == Chan],
    case Remain of
        [CPid] -> chan_manager:send_chan( CPid, {Msg, Chan, Cli} ),
                  ClientState;
        _ -> ClientState
    end.

perform_chan( _Msg, Cli, Chan, ChanState ) ->
    Prep = [$: | ChanState#cmanager.server_host]
            ++ ?RPL_WHOREPLY
            ++ [$  | Chan#chan.channame]
            ++ " ",
    Func = (fun({_Nick,ICli}, _) ->
                Msg = lists:concat( Prep,
                                    ICli#client.username, " ",
                                    ICli#client.host, " ",
                                    ChanState#cmanager.server_host,
                                    ICli#client.nick, ": " ),

                (Cli#client.send)( Cli#client.sendArgs, Msg )
            end),  
    ets:foldl(Func, 0, ChanState#chan.userlist),
    ets:foldl(Func, 0, ChanState#chan.foreignusers),
    ChanState.

