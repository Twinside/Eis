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
			validate_right( Chan, Cli, ClientState ),
	if 
		Valid ->
			case [Pid || {ChanName, Pid} <- Cli#client.is_in, ChanName == Chan] of
				[PPid] -> case [Pid || {ChanName, Pid} <- Dest#client.is_in, ChanName == Chan] of
							[_PPPid] -> 	chan_manager:send_chan( PPid, {Msg, Chan, Cli} );
							[] -> Errmsg = ?ERR_USERNOTINCHANNEL
								  ++ Dest#client.nick
								  ++ [$  | Chan ]
								  ++ [$  | ?ERR_USERNOTINCHANNEL_TXT],
								  irc:send_err( ClientState, Cli, Errmsg )
						  end;
				[] -> Errmsg = ?ERR_NOTONCHANNEL
	                  ++ Cli#client.nick
	                  ++ [$  | Chan ]
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
                 ++ [$  | Chan ]
                 ++ [$  | ?ERR_NOSUCHCHANNEL_TXT],
                 irc:send_err( ClientState, Cli, Errmsg )
	end.
	
% Check userright in the chan
validate_right (Chan, Cli, ClientState) ->
	Valid = irc_laws:check_chanlaw( 'KICK', chan_manager:get_user_right( Chan, Cli#client.nick ), []),
	if 
		Valid -> true;
		true  -> Errmsg = ?ERR_CHANOPPRIVSNEEDED
                 ++ [$  | Chan ]
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
