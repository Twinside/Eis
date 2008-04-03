%% @doc
%%	Module containing all the code related
%%	to the IRC notice command.
%% @end
-module( com_notice ).

-include( "irc_struct.hrl" ).

-vsn( p01 ).

-export([
			perform_client/3
		]).

perform_client(Msg, _From, _State) ->
	Lst = Msg#msg.params,
	Dest = 0, % à modif ici	
	case Lst of
		[Sender | Text] ->
					(Dest#client.send)((Sender#client.sendArgs), Text)
	end
.
