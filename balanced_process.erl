-module(balanced_process).

-export([behaviour_info/1]).

behaviour_info( callbacks ) ->
	[{add_ressource, 1},		% add a ressource to be managed by the process.
	 {take_any, 0},			% return a managed ressource and forget about it.
	 {kill_ressource, 1}];	% remove a managed ressource

behaviour_info(_Other) ->
	undefined.
	
