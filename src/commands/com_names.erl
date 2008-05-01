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
    if Secret -> "@ ";
       true -> Priv = irc_laws:is_chan_private( Chan ),
                if Priv -> "* ";
                   true -> "= "
                end
    end.

send_namelist( Chan, Cli, Servhost ) ->
    _Prefix = ":"
            ++ Servhost
            ++ ?RPL_NAMEREPLY
            ++ get_chan_prefix(Chan)
            ++ Chan#chan.channame
            ++ " :"
            ++ Cli#client.nick.
            
