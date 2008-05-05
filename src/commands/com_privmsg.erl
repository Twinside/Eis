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

%% @doc
%%  Called by the client listener, the first step is to
%%  validate the command, next is to try to send to a chan
%% @end
perform_client( Msg, Cli, ClientState ) ->
    ValidCom = validate_command( Msg, Cli, ClientState ),
    if not ValidCom -> ClientState;
       true -> [Dest|_] = Msg#msg.params,
                do_chan_send( Msg, Cli, ClientState, irc:is_channame_valid(Dest) ),
                ClientState
    end
    .

%% @doc
%% 
%% @end
validate_command( Msg, Cli, ClientState ) -> 
    check_recipient( Msg, Cli, ClientState ) andalso
    check_text( Msg, Cli, ClientState ).

%% @doc
%%  Verify that that the message got a real
%%  destination, if not send an error message/
%% @end
check_recipient( #msg{ params = [] }, Cli, State ) ->
    Errmsg = ?ERR_NORECIPIENT ++ ?ERR_NORECIPIENT_TXT,
    irc:send_err( State, Cli, Errmsg ),
    false;
check_recipient( _, _ ,_ ) -> true.
 
 %% @doc
 %%   Check that the message is not empty,
 %%   Send an error message otherwise.
 %% @end
check_text( #msg{ data= "" }, Cli, State ) ->
    Errmsg = ?ERR_NOTEXTTOSEND ++ ?ERR_NOTEXTTOSEND_TXT,
    irc:send_err( State, Cli, Errmsg ),
    false;
check_text( _, _, _ ) -> true.

%% @doc
%%  The last parameter indicate a valid chan name
%%  If invalid try to send to a client, otherwise,
%%  continue sending process
%% @end
do_chan_send( Msg, Cli, ClientState, false ) ->
    [Dest|_] = Msg#msg.params,
    do_cli_send( Msg, Cli, ClientState, irc:is_username_valid( Dest ) );
do_chan_send( Msg, Cli, State, true ) ->
    [Dest|_] = Msg#msg.params,
    case [P || {Name,P} <- Cli#client.is_in, Name == Dest] of
        [PidChan] -> chan_manager:send_chan( PidChan, {Msg, Dest, Cli} );
        
        [] -> Rez = server_node:get_chan( State#listener.servernode, Dest ),
              privmsg_to_wild( Rez , Msg, Cli, State, Dest )
    end.
 
%% @doc
%%  Try to send the message to a chan when
%%  the user is not inside, if no chan exists
%%  with that name, send an error message.
%% @end
privmsg_to_wild( {ok, Pid}, Msg, Cli, _State, Name ) ->
    chan_manager:send_chan( Pid, {Msg, Name, {notin_chan,Cli}} );
privmsg_to_wild( _, _Msg, Cli, State, Name ) ->
    Msgtxt = ?ERR_NOSUCHCHANNEL ++ Name ++ ?ERR_NOSUCHCHANNEL_TXT ++ Name ++ "\r\n",
    irc:send_err( State, Cli, Msgtxt ).

%% @doc
%%   Try to find the client, if not, send
%%   an error message. If the client is found
%%   directly send the message to him.
%% @end
do_cli_send( Msg, Cli, CliState, true ) ->
    [Nick|_] = Msg#msg.params,
    case server_node:get_client( CliState#listener.servernode, Nick) of
        {ok, DestCli} -> Tosend = irc:string_of_msg( irc:update_sender(Msg, Cli)),
                         (DestCli#client.send)(DestCli#client.sendArgs, Tosend);
        _ -> do_cli_send( Msg, Cli, CliState, false )
    end;
do_cli_send( #msg{ params=[Nick|_] }, Cli, CliState, _ ) ->
    ErrMsg = ?ERR_NOSUCHNICK ++ Nick ++ ?ERR_NOSUCHNICK_TXT ++ Nick ++ "\r\n",
    irc:send_err( CliState, Cli, ErrMsg ).

%% @doc
%%  Called by the chan_manager
%%  when the user not in chan, problematic...
%%  check for rights, if allowed, continue with
%%  normal behaviour, else send error.
%% @end
perform_chan( Msg, {notin_chan, Cli}, Chan, ChanState ) ->
    Allowed = not irc_laws:is_chan_inmsgonly( Chan ),
    if Allowed -> perform_chan( Msg, Cli, Chan, ChanState );
       true -> send_cannotsenderr( Chan, Cli, ChanState ),
               ChanState
    end;

%% @doc
%%  Handle a message to be broadcasted
%%  in the chan. Check some rights
%% @end
perform_chan( Msg, Cli, Chan, ChanState ) ->
    Right = chan_manager:get_user_right( Chan, Cli ),
    Allowed = irc_laws:check_chanlaw( 'PRIVMSG', Right, Chan#chan.mode ),
    if Allowed -> NeoMsg = irc:update_sender( Msg, Cli ),
                  chan_manager:broadcast_diff_users( Chan, irc:string_of_msg( NeoMsg ), Cli#client.nick );
       true -> send_cannotsenderr( Chan, Cli, ChanState )
    end,   
    ChanState.

%% @doc
%%  little helper to send an error message.
%% @end
send_cannotsenderr( Chan, Cli, State ) ->
    Errmsg = ?ERR_CANNOTSENDTOCHAN
            ++ Chan#chan.channame
            ++ ?ERR_CANNOTSENDTOCHAN_TXT ++ "\r\n",
    irc:send_err( State, Cli, Errmsg ).

