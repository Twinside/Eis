-module(chan_manager).

-behaviour(gen_server).

% export for the gen_server
-export([init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3]).

%-vsn(prealpha)


%%
% gen_server implementation
%%
init(_Args) ->
	undefined.

%
% Different call used by the load balancer.
%
handle_call( {addressource, ChanName}, _From, State ) ->
	undefined;
handle_call( {killressource, ChanName}, _From, State ) ->
	undefined;
handle_call( takeany, From, State ) ->
	undefined;
	
handle_call(_Request,_From, State) ->
	{noreply, State}.

handle_cast(_Request,_State) ->
	undefined.

handle_info(_Info,_State) ->
	undefined.

terminate(_Reason,_State) ->
	undefined.

code_change(_OldVsn,_State,_Extra) ->
	undefined.

