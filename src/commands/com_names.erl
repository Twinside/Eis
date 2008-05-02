%% @doc
%%  All the code related to the IRC NAMES command.
%% @end
-module( com_names ).

-include( "irc_struct.hrl" ).

-vsn( p01 ).

-export([
            perform_client/3
            ,perform_chan/4
        ]).

-export([
            send_namelist/3
        ]).
         
perform_client( _Msg, _Cli, ClientState ) ->
    ClientState.

perform_chan( _Msg, _Cli, _Chan, ChanState ) ->
    ChanState.

get_chan_prefix( Chan ) ->
    Secret = irc_laws:is_chan_secret( Chan ),
    if Secret -> " @ ";
       true -> Priv = irc_laws:is_chan_private( Chan ),
                if Priv -> " * ";
                   true -> " = "
                end
    end.

%% @doc
%%  Send the namelist to a client without using
%%  a message, can be used after a join to automatically
%%  send the nicklist to a client by example.
%% @end
%% @spec send_namelist( Channame, Cli, Serverhost ) -> true
%% where
%%      Chan = chan()
%%      Cli = client()
%%      Serverhost = string()
send_namelist( Chan, Cli, Servhost ) ->
    Prefix = ":"
            ++ Servhost
            ++ ?RPL_NAMEREPLY
            ++ Cli#client.nick
            ++ get_chan_prefix(Chan)
            ++ Chan#chan.channame
            ++ " :",
    Func = (fun({_Nick,{ICli, Rights}}, _) ->
                Cr = (case irc_laws:mode_prefix( Rights ) of
                        [] -> [];
                        A -> [A] end),
                        
                Msg = lists:concat( [
                                        Prefix,
                                        Cr,
                                        ICli#client.nick,
                                        "\r\n"
                                    ] ),

                (Cli#client.send)( Cli#client.sendArgs, Msg )
            end),  
    ets:foldl(Func, 0, Chan#chan.userlist),
    ets:foldl(Func, 0, Chan#chan.foreignusers),
    Finish = ?RPL_ENDOFNAMES
            ++ Cli#client.nick
            ++ [$ | Chan#chan.channame]
            ++ ?RPL_ENDOFNAMES_TXT,
    irc:send_err( Servhost, Cli, Finish ),
    true.
            
