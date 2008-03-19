-module( server_node ).

-behaviour( gen_server ).

-record( srvs,
		{
			clibal,		% load balancer for client
			chanbal	% load balancer for channels
		} ).

-export([
			is_client_existing/2
		]).

-vsn( p01 ).

is_client_existing( ServerPid, NickName ) ->
	false.
	
