%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	This module a tiny ident client conforming to the RFC 1413.
%% @end
%% @reference
%%	RFC <a href="http://www.faqs.org/rfcs/rfc1413.html">1413</a>
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module( ident_checker ).

-define( IDENT_PORT, 113 ).

-export( [check_ident/2] ).

-vsn( p01 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Ask to perform an ident request of a client. Every data used
%%	to make the call is deduced from the socket used to be
%%	connected to the client.
%% @end
%% 
%% @spec check_ident( ClientSocket, Timeout ) -> Result
%% where
%%		ClientSocket = socket()
%%		Timeout = int()
%%		Result = {error, Reason} | {ok, Answer}
%%			Answer = string()

check_ident( ClientSocket, Timeout ) ->
	case request_of_socket( ClientSocket, Timeout) of
		{error, Reason , _ } -> {error, Reason};
		{ok, Sock, Req} -> perform_request( Sock, Req, Timeout)
	end.

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
	
