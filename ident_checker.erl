-module( ident_checker ).

-define( IDENT_PORT, 113 ).

-export( [check_ident/2] ).

request_of_socket( Sock, Timeout ) ->
	{ ForeignAdress, ForeignPort } = inet:peername( Sock ),
	{ ok, LocalPort } = inet:port( Sock ),
	Request = list:concat([ integer_to_list(LocalPort),
							",",
							integer_to_list(ForeignPort)]),
	{Status, Val} = gen_tcp:connect( ForeignAdress, ?IDENT_PORT, [], Timeout ),
	{Status, Val, Request}.

parse_answer( Answer ) ->
	case string:tokens( Answer, ":" ) of
		[_, "ERROR" | _ ]	-> error;
		[_, "USERID", System, Data] -> { ok, {System, Data} }
	end.

perform_request( Sock, Req, Timeout ) ->
	gen_tcp:send( Sock, Req ),
	case gen_tcp:recv( Sock, 0, Timeout) of
		{error, _} -> error;
		{ok, Answer} -> parse_answer( Answer )
	end.
		
check_ident( ClientSocket, Timeout ) ->
	case request_of_socket( ClientSocket, Timeout) of
		{error, Reason , _ } -> {error, Reason};
		{ok, Sock, Req} -> perform_request( Sock, Req, Timeout)
	end.
	
