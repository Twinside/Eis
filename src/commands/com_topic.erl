-module( com_topic ).

-include( "irc_struct.hrl" ).
-vsn( p01 ).

-export([
		perform_client/3,
		perform_chan/4
		]).
		
perform_client( #msg { params=[] }, Cli, ClientState ) ->
    irc:send_err( ClientState, Cli, ?ERR_NEEDMOREPARAMS );
		
perform_client( Msg, Cli, ClientState ) ->
	[ _Chan, Topic ]  = Msg#msg.params,
	perform_topic( Msg, Cli, ClientState, Topic ),
	ClientState.
	
perform_topic ( Msg, Cli, ClientState, [] ) ->
	[ Chan | _ ]  = Msg#msg.params,
	Valid = com_kick:validate_chan( Chan, Cli, ClientState ),
	if 
		Valid -> 
				Remain = [Pid || {Name,Pid} <- Cli#client.is_in, Name == Chan],
				case Remain of
					[CPid] -> chan_manager:send_chan( CPid, {Msg, Chan, Cli} ),
							  ClientState;
					_ -> ClientState
				end	
	end;
	
perform_topic( Msg, Cli, ClientState, _Topic ) ->
	[ Chan | _ ]  = Msg#msg.params,
	Valid = com_kick:validate_chan( Chan, Cli, ClientState ) andalso
			validate_right( Chan, Cli, ClientState ),
	if 
		Valid -> 
				Remain = [Pid || {Name,Pid} <- Cli#client.is_in, Name == Chan],
				case Remain of
					[CPid] -> chan_manager:send_chan( CPid, {Msg, Chan, Cli} ),
							  ClientState;
					_ -> ClientState
				end	
	end.
	
validate_right (Chan, Cli, ClientState) ->
	Valid = irc_laws:check_chanlaw( 'TOPIC', chan_manager:get_user_right( Chan, Cli#client.nick ), []),
	if 
		Valid -> true;
		true  -> Errmsg = ?ERR_CHANOPPRIVSNEEDED
                 ++ [$  | Chan ]
                 ++ [$  | ?ERR_CHANOPPRIVSNEEDED_TXT],
                 irc:send_err( ClientState, Cli, Errmsg )
	end.	

perform_chan( Msg, Cli, Chan, ChanState ) ->
	[ Chan, Topic | _ ]  = Msg#msg.params,
	do_com_topic ( Chan, Cli, Topic ),
	ChanState.

% Check if the topic is setted
check_topic_set( [] ) -> false;
check_topic_set( _ )  -> true.
	
% Get topic
do_com_topic( Chan, Cli, [] ) ->
	TopicSet = check_topic_set( Chan#chan.topic ),
	if 
		TopicSet ->	Msg = ?RPL_TOPIC
					++ [$  | Chan#chan.channame]
					++ [$: | Chan#chan.topic ],
					(Cli#client.send)( Cli#client.sendArgs, Msg );
		true 	 -> Msg = ?RPL_NOTOPIC
					++ [$  | Chan#chan.channame]
					++ [$: | ?RPL_NOTOPIC_TXT ],
					(Cli#client.send)( Cli#client.sendArgs, Msg )
	end.
	
% Change topic
do_com_topic( Chan, _Cli, Topic ) ->
	Chan#chan { topic = Topic }.
	