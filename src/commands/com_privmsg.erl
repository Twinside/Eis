%% @doc
%%	Command containing all the code related to
%%	the IRC PRIVMSG command.
%% @end
-module( com_privmsg ).

-vsn( p01 ).

-export([
            perform_client/3
            ,perform_chan/4
        ]).

perform_client( Msg, Cli, ClientState ) ->
    _ValidCom = validate_command( Msg, Cli, ClientState ),
    ClientState
    .

validate_command( _Msg, _Cli, _ClientState ) -> false.

perform_chan( _Msg, _Cli, _Chan, ChanState ) -> ChanState.
