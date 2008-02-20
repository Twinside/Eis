-module(client_listener).

-behaviour(gen_server).
-behaviour(balanced_process).

% export for the balanced_process
-export([add_ressource/1,
			take_any/0,
			kill_ressource/1]).

% export for the gen_server
-export([init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3]).

%-vsn(prealpha)

%%
% balanced_process implementation
%%
add_ressource( Client ) ->
	undefined.

take_any() ->
	undefined.

kill_ressource( Client ) ->
	undefined.

%%
% gen_server implementation
%%
init(_Args) ->
	undefined.
	
handle_call(_Request,_From,_State) ->
	undefined.

handle_cast(_Request,_State) ->
	undefined.

handle_info(_Info,_State) ->
	undefined.

terminate(_Reason,_State) ->
	undefined.

code_change(_OldVsn,_State,_Extra) ->
	undefined.

