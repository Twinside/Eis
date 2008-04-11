%% @doc
%%	Module containing all the code related
%%	to the IRC notice command.
%% @end
-module( com_notice ).

-include( "irc_struct.hrl" ).

-vsn( p01 ).

-export([
			perform_client/3
            ,perform_chan/4
		]).

perform_client(Msg, From, State) ->
	Lst = Msg#msg.params,
    NeoMess = irc:update_sender( Msg, From ),
	
    case Lst of
		[Dest |_ ] -> send_notice( NeoMess, Dest, State, irc:is_username_valid( Dest ) ),
                      State;
         
         _ -> State % just ignore           
	end.

send_notice( Msg, Dst, State, true ) ->
    case server_node:get_client( State#listener.servernode, Dst ) of
        {ok, Cli} -> Notice = irc:string_of_msg( Msg ),
                     (Cli#client.send)(Cli#client.sendArgs, Notice),
                     ok;
        _ -> ok
    end;
send_notice( _, _, _, _ ) ->
    ok.
    
perform_chan( _Msg, _Cli, _Chan, ChanState ) -> ChanState.

