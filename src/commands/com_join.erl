%% @doc
%%	Module taking in charge all the code related
%%	to the irc JOIN command.
%% @end
-module( com_join ).

-vsn( p01 ).

-export([
			perform_client/3,
			perform_chan/3
		]).

perform_client( _Cli, ClientState, _IrcMsg ) ->
	ClientState.


perform_chan( _Chan, ChanState, _IrcMsg ) ->
	ChanState.

