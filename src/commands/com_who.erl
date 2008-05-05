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

%% @doc
%%   called by the client_listener,
%%   perform basic command validation
%% @end
perform_client( Msg, Cli, ClientState ) ->
    case Msg#msg.params of
        [_Chan|_] -> analyse( Msg, Cli, ClientState );
        _ -> ClientState
    end.

%% @doc
%%  Try to find the chan in parameter, no
%%  wildcard expression is used. If found
%%  send message to that process.
%% @end
analyse( Msg, Cli, ClientState ) ->
    [Chan|_] = Msg#msg.params,
    Remain = [Pid || {Name,Pid} <- Cli#client.is_in, Name == Chan],
    case Remain of
        [CPid] -> chan_manager:send_chan( CPid, {Msg, Chan, Cli} ),
                  ClientState;
        _ -> ClientState
    end.

%% @doc
%%  Called by the chan manager, send all the info
%%  about the client to the caller.
%% @end
perform_chan( _Msg, Cli, Chan, ChanState ) ->
    Prep = [$: | ChanState#cmanager.server_host]
            ++ ?RPL_WHOREPLY
            ++ Cli#client.nick
            ++ [$ | Chan#chan.channame]
            ++ " ",
    Func = (fun({_Nick,{ICli, Rights}}, _) ->
                Cr = (case irc_laws:mode_prefix( Rights ) of
                        [] -> [];
                        A -> [A] end),
                        
                Msg = lists:concat( [Prep,
                                    [$~ |ICli#client.username],
                                    [$  |ICli#client.host],
                                    [$  |ChanState#cmanager.server_host],
                                    [$  |ICli#client.nick],
                                    " H",
                                    Cr,
                                    " :0 no_info\r\n"] ),

                (Cli#client.send)( Cli#client.sendArgs, Msg )
            end),  
    ets:foldl(Func, 0, Chan#chan.userlist),
    ets:foldl(Func, 0, Chan#chan.foreignusers),
    Finish = ?RPL_ENDOFWHO
            ++ Cli#client.nick
            ++ [$ | Chan#chan.channame]
            ++ ?RPL_ENDOFWHO_TXT,
    irc:send_err(ChanState, Cli, Finish ),
    ChanState.

