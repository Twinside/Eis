%% @doc
%%	Module containing all the code related
%%	to the IRC Kick command.
%% @end
-module(com_kick).

-include( "irc_struct.hrl" ).

-export([
            perform_client/3,
	    perform_chan/4
        ]).

-vsn( p01 ).


perform_client( _Msg, _Cli, ClientState ) ->
	ClientState.

perform_chan( Msg, Cli, Chan, ChanState ) ->
	MsgTosend = prepare_sended_text( Msg, Cli ),
	CheckRight = check_granting( ?MCHANOP, Cli#client.rights ),	
	if 
		CheckRight ->
			[ Dest|_ ] = Msg#msg.params,
			Ok = irc:is_username_valid(Dest),
			if 
				Ok ->
					case com_part:cleanup_chan( Chan, Dest, ChanState ) of
						{ removed, NeoState } -> NeoState;
						{ ok, State } ->
							[ { _,NeoChan } ] = ets:lookup( State#cmanager.byname, Chan#chan.channame ),
							chan_manager:broadcast_users( NeoChan, MsgTosend ),
							State
					end
			end
	end;

perform_chan( _, _, _, ChanState ) ->
	ChanState.
	
prepare_sended_text( Msg, Cli ) ->
    NeoMessage = irc:update_sender( Msg, Cli ),
    irc:string_of_msg( NeoMessage ).
	
