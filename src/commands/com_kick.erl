%% @doc
%%	Module containing all the code related
%%	to the IRC Kick command.
%% @end
-module(com_kick).

-include( "irc_struct.hrl" ).
-vsn( p01 ).

-export([
		perform_client/3,
		perform_chan/4
		]).

perform_client( #msg { params=[] }, Cli, ClientState ) ->
    irc:send_err( ClientState, Cli, ?ERR_NEEDMOREPARAMS );

perform_client( #msg { params=[_] }, Cli, ClientState ) ->
    irc:send_err( ClientState, Cli, ?ERR_NEEDMOREPARAMS );
        		
perform_client( Msg, Cli, ClientState) ->
	[ Chan, _ | _ ]  = Msg#msg.params,
    UMsg = irc:update_sender( Msg, Cli ),
    case [Pid || {ChanName, Pid} <- Cli#client.is_in, ChanName == Chan] of
        [PPid] -> chan_manager:send_chan( PPid, {UMsg, Chan, Cli} );
        []     -> err:notonchannel( ClientState, Cli, Chan )
    end,
    ClientState.
	
% Check userright in the chan
validate_right (Chan, Cli, State) ->
    URight = chan_manager:get_user_right( Chan, Cli#client.nick ),
	Valid = irc_laws:check_chanlaw( 'KICK', URight, Chan#chan.mode ),
	if 
		Valid -> true;
		true  -> err:chanopprivsneeded( State, Cli, Chan ),
                 false
	end.
	
		
perform_chan( Msg, Cli, Chan, ChanState ) ->
    Valid = validate_right( Chan, Cli, ChanState ),
    if Valid -> [ _, Target | _ ]  = Msg#msg.params,
                AssumedCli = ets:lookup( Chan#chan.userlist, Target ),
                kick_user( Msg, AssumedCli, Cli, Chan, ChanState );
                
       true -> ChanState
    end.

kick_user( Msg, [{_, {Cli, _}}], _Kicker, Chan, ChanState ) ->
    StrMsg = irc:string_of_msg( Msg ),
    chan_manager:broadcast_users( Chan, StrMsg ),
    {_, State} = com_part:cleanup_chan( Chan, Cli, ChanState ),
    State;
    
kick_user( #msg { params = [_, TargetNick |_]}, _, Kicker,
            Chan, ChanState ) ->
    err:usernotinchannel( ChanState, Kicker, TargetNick, Chan ),
    ChanState.
    
