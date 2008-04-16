-module( com_mode ).

-include( "irc_struct.hrl" ).

-export([
            perform_client/3
            ,perform_chan/4
        ]).

-vsn( p01 ).


perform_client( _Msg, _Cli, ClientState ) ->
    ClientState.

perform_chan( _Msg, _Data, _Chan, ChanState ) ->
    ChanState.

