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
		
perform_client( Msg, Cli, ClientState) ->
	[ Chan, Dest | _ ]  = Msg#msg.params,
	Valid = validate_chan( Chan, Cli, ClientState ) andalso
			validate_right( Chan, Cli#client.nick, ClientState ),
	if 
		Valid ->
			case [Pid || {ChanName, Pid} <- Cli#client.is_in, ChanName == Chan] of
				[PPid] -> case [Pid || {ChanName, Pid} <- Dest#client.is_in, ChanName == Chan] of
							[PPPid] -> 	chan_manager:send_chan( PPid, {Msg, ChanName, Cli} );
							[] -> Errmsg = ?ERR_USERNOTINCHANNEL
								  ++ Dest#client.nick
								  ++ [$  | ChanName ]
								  ++ [$  | ?ERR_USERNOTINCHANNEL_TXT],
								  irc:send_err( ClientState, Cli, Errmsg )
						  end;
				[] -> Errmsg = ?ERR_NOTONCHANNEL
	                  ++ Cli#client.nick
	                  ++ [$  | ChanName ]
	                  ++ [$  | ?ERR_NOTONCHANNEL_TXT],
	                  irc:send_err( ClientState, Cli, Errmsg )
			end
	end.

% Check if the chan exist
validate_chan ( Chan, Cli, ClientState ) ->
	Valid = irc:is_channame_valid( Chan ),
	if
		Valid -> true;
		true  -> Errmsg = ?ERR_NOSUCHCHANNEL
                 ++ Cli#client.nick
                 ++ [$  | ChanName ]
                 ++ [$  | ?ERR_NOSUCHCHANNEL_TXT],
                 irc:send_err( ClientState, Cli, Errmsg )
	end.
	
% Check userright in the chan
validate_right (Chan, Nick, ClientState) ->
	Valid = irc_laws:check_chanlaw( 'KICK', chan_manager:get_user_right( Chan, Nick ), []),
	if 
		Valid -> true;
		true  -> Errmsg = ?ERR_CHANOPPRIVSNEEDED
                 ++ [$  | ChanName ]
                 ++ [$  | ?ERR_CHANOPPRIVSNEEDED_TXT],
                 irc:send_err( ClientState, Cli, Errmsg )
	end.
	
		
perform_chan( Msg, _Cli, Chan, ChanState ) ->
	[ Chan, Dest | _ ]  = Msg#msg.params,
	StrMsg = irc:string_of_msg( Msg ),
    chan_manager:broadcast_users( Chan, StrMsg ),
    {_, State} = com_part:cleanup_chan( Chan, Dest, ChanState ),
    State
    .