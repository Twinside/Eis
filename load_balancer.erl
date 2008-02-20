-module (load_balancer).

-behaviour(supervisor).

% export for the behaviour.
-export([start_link/0]).
-export([init/1]).

start_link(Module, Initiator, ) ->
	supervisor:start_link(?MODULE, []).

init( _Args ) ->
	{ok,
		{						% restart a process for each dead, and stop
			{one_for_one, 1,60},% if more than 1 process stop in 60 seconds.
			[{}]
		}.

