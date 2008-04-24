-module( com_topic ).

-include( "irc_struct.hrl" ).

-vsn( p01 ).

%%-export([
%%            perform_client/3
%%            ,perform_chan/4
%%        ]).

%%perform_client( Msg, Cli, ClientState ) ->
%%	Tosend = prepare_sended_text( Msg, Cli ),
%%	Dispatcher = (fun( {Chan,Pid}, _ ) ->
%%                  chan_manager:send_chan( Pid, {Msg, Chan, {Tosend, Cli}} )
%%                   end),
%% 	lists:foldl( Dispatcher, 0, Cli#client.is_in ),
%%        ClientState.

%%perform_chan( Msg, {MsgTosend, User}, Chan, ChanState ) ->
%%	CheckRight = check_granting( ?MCHANOP, chan_manager:get_user_right( Chan, Cli#client.nick ) ),	
%%	if Checkright ->
%%		[ _Dest| _Command | [Subject | _] ] = Msg#msg.params,
%%		Ok = irc:is_channame_valid(Chan#chan.channame),

	%% PAS FINIT
%%	end.
