%% @doc
%%  Module in charge of the implementation of the mode
%%  command. Should be edited with irc_laws.erl in parallel
%%  com_mode use heavily irc_laws.
%% @end
-module( com_mode ).

-include( "irc_struct.hrl" ).

-export([
            perform_client/3
            ,perform_chan/4
        ]).

-vsn( p01 ).


perform_client( Msg, Cli, ClientState ) ->
    msg_dispatch( Msg, Cli, ClientState, Msg#msg.params ).

msg_dispatch( _Msg, Cli, State, [] ) ->
    irc:send_err( State, Cli, ?ERR_NEEDMOREPARAMS),
    State;
    
msg_dispatch( Msg, Cli, State, [Name|_] ) ->
    Is_chan = irc:is_channame_valid( Name ),
    if Is_chan -> chan_request( Msg, Cli, State );
       true ->
            ValidNick = irc:is_username_valid( Name ),
            if ValidNick -> client_request( Msg, Cli, State );
                true -> send_wrongnick( State, Cli, Name )
            end
    end.

send_wrongnick( State, Cli, Name ) ->
    Msg = ?ERR_NOSUCHNICK ++ Name ++ ?ERR_NOSUCHNICK_TXT,
    irc:send_err( State, Cli, Msg ),
    State.

%% @doc
%% if the mode command concern a client.
%% @end
client_request( #msg{ params =[Nick] }, Cli, State ) ->
    case server_node:get_client( State#listener.servernode, Nick ) of
        {ok, Ncli} ->
                Smode = irc_laws:global_mode_to_string( Ncli#client.rights ),
                Msg = ?RPL_UMODEIS ++ Smode ++ "\r\n",
                irc:send_err( State, Cli, Msg ),
                State;
        _ -> send_wrongnick( State, Cli, Nick )
    end;
    
client_request( #msg{ params = [Nick,Modif|_] }, Cli, State ) ->
    if Nick /= Cli#client.nick ->
           Msg = ?ERR_USERDONTMATCH ++ ?ERR_USERDONTMATCH_TXT,
           irc:send_err( State, Cli, Msg );
           
       true ->
            case irc_laws:string_to_globalmode( Modif ) of
                {Side,Mode} -> 
                    Valid = irc_laws:check_granting( Mode, Cli#client.rights ),
                    apply_user_mode( Cli, State, Valid, Side, Mode );
                       
                {unknwon,_,C} -> Msg = ?ERR_UNKNOWNMODEFLAG
                                    ++ ?ERR_UNKNOWNMODEFLAG_TXT
                                    ++ [C|"\r\n"],
                                 irc:send_err( State, Cli, Msg)
            end
    end,       
    State.

%% @doc
%%  Realy the mode to the user or
%%  send the apropriate error message
%% @end
apply_user_mode( Cli, State, false, _, _ )->
    Msg = ?ERR_NOPRIVILEGES ++ ?ERR_NOPRIVILEGES_TXT,
    irc:send_err( State,Cli, Msg ),
    State;

apply_user_mode( Cli, State, true, $+, Mode ) ->
    NeoCli = Cli#client{ rights = Cli#client.rights bor Mode },
    ets:insert( State#listener.bynick, {NeoCli#client.nick, NeoCli} ),
    State;

apply_user_mode( Cli, State, true, $-, Mode ) ->
    NeoCli = Cli#client{ rights = Cli#client.rights band (bnot Mode) },
    ets:insert( State#listener.bynick, {NeoCli#client.nick, NeoCli} ),
    State
    .
    
chan_request( Msg, Cli, State ) ->
    [Name|_] = Msg#msg.params,
    case lists:keysearch( Name, 1, Cli#client.is_in ) of
        {value, {_, Pid}} -> UMsg = irc:update_sender( Msg, Cli ),
                             chan_manager:send_chan( Pid, {UMsg, Name, Cli} );
        _ -> Msg = ?ERR_NOTONCHANNEL
                    ++ Name
                    ++ ?ERR_NOTONCHANNEL_TXT
                    ++ Name,
            irc:send_err( State, Cli, Msg )
    end,
    State.

perform_chan( Msg, Cli, Chan, State ) ->
    case Msg#msg.params of
        [_Name] -> send_chan_info( Cli, Chan, State );
        [_Name,"+b"] -> send_ban_list( Cli, Chan, State );
        [_Name,What] -> apply_simple( irc_laws:string_to_usermode( What ), Msg, Cli, Chan, State );
        [_Name,What|Arg] -> apply_arg( irc_laws:string_to_usermode( What ), Arg, Msg, Cli, Chan, State );
        _ -> State
    end.

apply_simple( {Side,What}, Msg, Cli, Chan, State ) ->
    Needmore = irc_laws:is_chan_passworded( What ) or
                irc_laws:is_chan_limited( What ),
    if Needmore ->
            EMsg = ?ERR_NEEDMOREPARAMS,
            irc:send_err( State, Cli, EMsg ),
            State;
            
       true ->
            make_modechange( Msg, Side, What, Chan, State )
    end;

apply_simple( _, _Msg, Cli, _Chan, State ) ->
    Msg = ?ERR_UNKNOWNMODEFLAG ++ ?ERR_UNKNOWNMODEFLAG_TXT ++ "\r\n",
    irc:send_err( State, Cli, Msg ),
    State.

    
make_modechange( Msg, $+, What, Chan, State ) ->
    Nch = Chan#chan{ mode = Chan#chan.mode bor What },
    SMsg = irc:string_of_msg( Msg ),
    ets:insert( State#cmanager.byname, {Nch#chan.channame, Nch} ),
    chan_manager:broadcast_users( Chan, SMsg ),
    State;
    
make_modechange( Msg, $-, What, Chan, State ) ->
    Nch = Chan#chan{ mode = Chan#chan.mode band (bnot What) },
    SMsg = irc:string_of_msg( Msg ),
    ets:insert( State#cmanager.byname, {Nch#chan.channame, Nch} ),
    chan_manager:broadcast_users( Chan, SMsg ),
    State.
    
        
apply_arg( {$+, Mode}, _Arg, _Msg, _Cli, _Chan, State ) ->
    _Limit = irc_laws:is_chan_limited( Mode ),
    State;
apply_arg( {$-, _Mode}, _Arg, _Msg, _Cli, _Chan, State ) ->
    State;
apply_arg( {_Side, _What}, _Arg, _Msg, _Cli, _Chan, State ) ->
    State.

send_ban_list( Cli, Chan, State ) ->
    Msg = ?RPL_BANLIST ++ Chan#chan.channame ++ " ",
    Snd = (fun(Ban,_) ->
                FMsg = Msg ++ Ban ++ "\r\n",
                (Cli#client.send)(Cli#client.sendArgs, FMsg)
           end),
    lists:foldl( Snd, Cli, Chan#chan.banlist ),
    Endmsg = ?RPL_ENDOFBANLIST
            ++ Chan#chan.channame
            ++ ?RPL_ENDOFBANLIST_TXT,
    irc:send_err( State, Cli, Endmsg ),
    State.

make_pass_info( Chan, Str ) ->
    Pass = irc_laws:is_chan_passworded( Chan ),
    if Pass -> make_limit_info( Chan, Str ++ [$  |Chan#chan.password] );
       true -> make_limit_info( Chan, Str )
    end.

make_limit_info( Chan, Str ) ->
    Limit = irc_laws:is_chan_limited( Chan ),
    if Limit -> Str ++ [$  |integer_to_list( Chan#chan.userlimit )];
       true  -> Str
    end.

send_chan_info( Cli, Chan, State ) ->
    Mode = irc_laws:chan_mode_to_string( Chan#chan.mode ) ++
            make_pass_info( Chan, "" ),
    Msg = ?RPL_CHANNELMODEIS
        ++ Chan#chan.channame
        ++ [$  |Mode],
    irc:send_err( State, Cli, Msg ),
    State.
    
