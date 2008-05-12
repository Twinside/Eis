%% @doc
%%  Module in charge of the implementation of the mode
%%  command. Should be edited with irc_laws.erl in parallel
%%  com_mode use heavily irc_laws.
%% @end
-module( com_mode ).

-include( "transaction.hrl" ).
-include( "irc_struct.hrl" ).

-export([
            perform_client/3
            ,perform_chan/4
        ]).

-vsn( p01 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   On the client side
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
perform_client( Msg, Cli, ClientState ) ->
    msg_dispatch( Msg, Cli, ClientState, Msg#msg.params ).

% case when there is not enough param
msg_dispatch( _Msg, Cli, State, [] ) ->
    irc:send_err( State, Cli, ?ERR_NEEDMOREPARAMS),
    State;

% try to determine the application
% - user with a nick
% - chan
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
    Msg = ?ERR_NOSUCHNICK ++ Name ++ [?ERR_NOSUCHNICK_TXT|"\r\n"],
    irc:send_err( State, Cli, Msg ),
    State.

%% @doc
%%  if the mode command concern a client.
%%  First case : to request the client's mode
%% @end
client_request( #msg{ params =[Nick] }, Cli, State ) ->
    case server_node:get_client( State#listener.servernode, Nick ) of
        {ok, Ncli} ->
                Smode = irc_laws:global_mode_to_string( Ncli#client.rights ),
                Msg = ?RPL_UMODEIS ++ [Smode | "\r\n"],
                irc:send_err( State, Cli, Msg ),
                State;
        _ -> send_wrongnick( State, Cli, Nick )
    end;

% Second case :
% to apply a modification to the client.
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
  ?TRANSACTIONBEGIN
    ets:insert( State#listener.bynick, {NeoCli#client.nick, NeoCli} )
  ?TRANSACTIONEND,
    State;

apply_user_mode( Cli, State, true, $-, Mode ) ->
    NeoCli = Cli#client{ rights = Cli#client.rights band (bnot Mode) },
  ?TRANSACTIONBEGIN    
    ets:insert( State#listener.bynick, {NeoCli#client.nick, NeoCli} )
  ?TRANSACTIONEND,  
    State
    .

%% @doc
%%  When the mode is concerning a chan,
%%  check presence and change of process.
%% @end
chan_request( Msg, Cli, State ) ->
    [Name|_] = Msg#msg.params,
    case lists:keysearch( Name, 1, Cli#client.is_in ) of
        {value, {_, Pid}} -> UMsg = irc:update_sender( Msg, Cli ),
                             chan_manager:send_chan( Pid, {UMsg, Name, Cli} );
        _ -> Msg = ?ERR_NOTONCHANNEL
                    ++ Name
                    ++ ?ERR_NOTONCHANNEL_TXT
                    ++ [Name | "\r\n"],
            irc:send_err( State, Cli, Msg )
    end,
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   On the chan side
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record( ms,
        {
            msg,
            cli,
            chan,
            state,
            right
       }).
       
perform_chan( Msg, Cli, Chan, State ) ->
    Applyer = (fun( M, Acc ) ->
                    case M of
                        {unknown,_} -> Acc;
                        {_, Mode, _} -> valid( M, Acc, Mode );
                        {_, Mode}    -> valid( M, Acc, Mode )
                    end end),
    case Msg#msg.params of
        [_Name] -> send_chan_info( Cli, Chan, State );
        [_Name,"+b"] -> send_ban_list( Cli, Chan, State );
        [_Name,What | Params] -> Modes = irc_laws:string_to_usermode( What ),
                                 [{_, {_,Right}}] = ets:lookup( Chan#chan.userlist, Cli#client.nick ),
                                 Mos = #ms {msg = Msg,
                                            cli = Cli,
                                            chan = Chan,
                                            right = Right,
                                            state = State },
                                 {_,NMos} = lists:foldl( Applyer, {Params, Mos}, Modes ),
                                 NMos#ms.state;
        _ -> State
    end.

%% @doc
%%  Given an ModeState and the right
%%  to apply, tell if it's valid or not.
%%  Also send an error message.
%% @end
valid( What, {Params, Mos}, R ) ->
    Right = Mos#ms.right,
    Valid = irc_laws:check_granting( R, Right ),
    if Valid -> apply_chan( What, {Params, Mos} );
       true -> ErrMsg = ?ERR_CHANOPPRIVSNEEDED
                        ++ (Mos#ms.cli)#client.nick
                        ++ ?ERR_CHANOPPRIVSNEEDED_TXT,
               irc:send_err( Mos#ms.state, Mos#ms.cli, ErrMsg ),
               {Params, Mos}
    end.

apply_chan( {unknown, C}, {Params, Mos} ) ->
    Msg = ?ERR_UNKNOWNMODEFLAG
        ++ ?ERR_UNKNOWNMODEFLAG_TXT
        ++ [C | "\r\n"],
    irc:send_err( Mos#ms.state, Mos#ms.cli, Msg ),
    {Params, Mos};
    
apply_chan( {$+, Mode, limit}, {[P|Next], Mos} ) ->
    case chars:int_from_string( P ) of
        error -> irc:send_err( Mos#ms.state, Mos#ms.cli, ?ERR_NEEDMOREPARAMS ),
                 {Next, Mos};
        Val -> OChan = Mos#ms.chan,
                Chan = OChan#chan { mode = OChan#chan.mode bor Mode,
                                    userlimit = Val },
                commit_change( Mos#ms.msg, Chan, {[Next], Mos} )
    end;

apply_chan( {$+, _, _}, {[], Mos} ) ->
    irc:send_err( Mos#ms.state, Mos#ms.cli, ?ERR_NEEDMOREPARAMS ),
    {[], Mos};
               
apply_chan( {$-, Mode, limit}, {Params, Mos} ) ->
    OChan = Mos#ms.chan,
    Chan = OChan#chan { mode = OChan#chan.mode band (bnot Mode),
                        userlimit = 0 },
    commit_change( Mos#ms.msg, Chan, {Params, Mos} );
    
apply_chan( {$+, Mode, key}, {[Key|Next], Mos} ) ->
    % Assume key must be a valid nickname,
    % this is made to avoid scary problem
    % of impossible nickname...
    Valid = irc:is_username_valid( Key ),
    if Valid -> OChan = Mos#ms.chan,
                Chan = OChan#chan { mode = OChan#chan.mode bor Mode,
                                    password = Key },
                commit_change( Mos#ms.msg, Chan, {Next, Mos} );
        true -> {Next, Mos}
    end;
    
apply_chan( {$-, Mode, key}, {Params, Mos} ) ->
    OChan = Mos#ms.chan,
    Chan = OChan#chan { mode = OChan#chan.mode band (bnot Mode),
                        password = "" },
    commit_change( Mos#ms.msg, Chan, {Params, Mos} ),
    {Params, Mos};

apply_chan( {$+, _, ban}, {[Ban|Next], Mos} ) ->
    Chan = Mos#ms.chan,
    State = Mos#ms.state,
    Valid = lists:length( Chan#chan.banlist ) + 1 < State#cmanager.max_ban_per_chan and
            (lists:length( Ban ) =< State#cmanager.max_ban_length),
    if Valid ->
            NeoChan = Chan#chan{ banlist = [Ban|Chan#chan.banlist] },
            commit_change( Mos#ms.msg, NeoChan, {Next, Mos} );

       true -> {Next, Mos}
    end;

apply_chan( {$-, Mode, ban}, {[Ban|Next], Mos} ) -> 
    OChan = Mos#ms.chan,
    Chan = OChan#chan { mode = OChan#chan.mode band (bnot Mode),
                        banlist = [Mask || Mask <- OChan#chan.banlist, Mask /= Ban] },
    commit_change( Mos#ms.msg, Chan, {Next, Mos} );

% assume to apply something to a nick only !!
apply_chan( {$+, Mode, _}, {[Name|Next], Mos} ) ->
    Chan = Mos#ms.chan,
    {Nick, {Cli,Right}} = ets:lookup( Chan#chan.userlist, Name ),
    ets:insert( Chan#chan.userlist, {Nick, {Cli, Right bor Mode}} ),
    {Next, Mos};

apply_chan( {$-, Mode, _}, {[Name|Next], Mos} ) ->
    Chan = Mos#ms.chan,
    {Nick, {Cli,Right}} = ets:lookup( Chan#chan.userlist, Name ),
    ets:insert( Chan#chan.userlist, {Nick, {Cli, Right band (not Mode)}} ),
    {Next, Mos};
    
apply_chan( {$-, What}, {Params, Mos} ) ->
    OChan = Mos#ms.chan,
    Chan = OChan#chan{ mode = (Mos#ms.chan)#chan.mode band (bnot What)},
    commit_change( Mos#ms.msg, Chan, {Params, Mos} );
    
apply_chan( {$+, What}, {Params, Mos} ) ->
    OChan = Mos#ms.chan,
    Chan = OChan#chan{ mode = (Mos#ms.chan)#chan.mode bor What },
    commit_change( Mos#ms.msg, Chan, {Params,Mos} );
    
apply_chan( _, Acc ) -> Acc.

commit_change( Msg, Chan, {Params, Mos} ) ->
    SMsg = irc:string_of_msg( Msg ),
    State = Mos#ms.state,
  ?TRANSACTIONBEGIN
    ets:insert( State#cmanager.byname, {Chan#chan.channame, Chan} )
  ?TRANSACTIONEND,
    chan_manager:broadcast_users( Chan, SMsg ),
    {Params, Mos}.

%% @doc
%%  Send the ban list in responce of MODE +b command.
%% @end
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

%% @doc
%%  Make a string with oprtional chan parameters.
%%  It's used to send the mode information.
%% @end
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
    Mode = irc_laws:chan_mode_to_string( Chan#chan.mode )
         ++ make_pass_info( Chan, "" ),
    Msg = ?RPL_CHANNELMODEIS
        ++ Cli#client.nick
        ++ " "
        ++ Chan#chan.channame
        ++ [$  |Mode]
        ++ "\r\n",
    irc:send_err( State, Cli, Msg ),
    State.
    
