%% @doc
%%	Module taking in charge all the code related
%%	to the irc JOIN command.
%% @end
-module( com_join ).

-include( "irc_struct.hrl" ).

-vsn( p01 ).

-export([
			perform_client/3
			,perform_chan/3
			,server_add/3
		]).

perform_client( Msg, Cli, ClientState ) ->
	Lst = Msg#msg.params,
	NeoMsg = irc:update_sender( Msg, Cli#client.nick ),
	ServerNode = ClientState#listener.servernode,

	case Lst of
		[] -> Args = Cli#client.sendArgs,
					ParamErr = irc:prepare_err(ClientState#listener.server_host,
												?ERR_NEEDMOREPARAMS),
					(Cli#client.send)( Args, ParamErr );
		[Dest | Next] ->
				(case server_node:get_chan(ServerNode, Dest) of
					error -> create_chan( NeoMsg, Cli, ServerNode );
					{ok, Chan} -> join_chan( NeoMsg, Cli, Chan, Next )
				end)
	end,
	ClientState.

join_chan( Msg, Cli, Chan, [] ) ->
	chan_manager:send_chan( Chan, {Msg, Cli, ""} )
	;

join_chan( Msg, Cli, Chan, [Pass] ) ->
	chan_manager:send_chan( Chan, {Msg, Cli, Pass} )
	.
	
create_chan( Msg, Cli, ServerNode ) ->
	[ChanName|Other] = Msg#msg.params,
	Chan = #chan { channame = ChanName,
					userlimit = no_limit,
					banlist = [],
					topic = "",
					mode = 0 },
	server_node:add_chan( ServerNode, Chan, Cli )
	.

perform_server( Chan, Serverstate, _IrcMsg ) ->
	Serverstate.
	
perform_chan( _Chan, ChanState, _IrcMsg ) ->
	ChanState.

server_add( ServerState, Chan, Owner ) ->
	Bal = ServerState#srvs.chanbal,
	{ok, Pid} = load_balancer:add_ressource( Bal, Chan ),
	gen_server:cast( Pid, {Chan,Owner} )
	.

