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
        
perform_client( _Msg, _Cli, ClientState ) ->
    ClientState.

perform_chan( _Msg, _Cli, _Chan, ChanState ) ->
    ChanState.

