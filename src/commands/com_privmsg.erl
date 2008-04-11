%% @doc
%%	Command containing all the code related to
%%	the IRC PRIVMSG command.
%% @end
-module( com_privmsg ).

-vsn( p01 ).
-include( "irc_struct.hrl" ).

-export([
            perform_client/3
            ,perform_chan/4
        ]).

perform_client( Msg, Cli, ClientState ) ->
    _ValidCom = validate_command( Msg, Cli, ClientState ),
    ClientState
    .

validate_command( Msg, Cli, ClientState ) -> 
    check_recipient( Msg, Cli, ClientState ) andalso
    check_text( Msg, Cli, ClientState ).

check_recipient( #msg{ params = [] }, Cli, State ) ->
    Errmsg = ?ERR_NORECIPIENT ++ ?ERR_NORECIPIENT_TXT,
    irc:send_err( State, Cli, Errmsg ),
    false;
check_recipient( _, _ ,_ ) -> true.
 
check_text( #msg{ data= "" }, Cli, State ) ->
    Errmsg = ?ERR_NOTEXTTOSEND ++ ?ERR_NOTEXTTOSEND_TXT,
    irc:send_err( State, Cli, Errmsg ),
    false;
check_text( _, _, _ ) -> true.

perform_chan( _Msg, _Cli, _Chan, ChanState ) -> ChanState.
