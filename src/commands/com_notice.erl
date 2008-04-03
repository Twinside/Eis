%% @doc
%%	Module containing all the code related
%%	to the IRC notice command.
%% @end
-module( com_notice ).
-vsn( p01 ).

-export	( 
		[notice/2]
		).

notice(Msg, Dest) ->
	Lst = Msg#msg.params,
	
	case Lst of
		[Sender | Text] ->
					(Dest#client.send)((Sender#client.sendArg), Text)
	end
.