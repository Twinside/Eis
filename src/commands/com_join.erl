%% @doc
%%	Module taking in charge all the code related
%%	to the irc JOIN command.
%% @end
-module( com_join ).

-vsn( p01 ).

-export([
			perform_client/2,
			perform_chan/2
		]).

perform_client( ClientState, _IrcMsg ) ->
	ClientState.


perform_chan( ChanState, _IrcMsg ) ->
	ChanState.

