%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	This module represent the logging thread which
%%	store all the diferents elements which happen
%%	across the server
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module (irc_log).

-define( LogVerbose, 0).
-define( LogInfo,  1 ).
-define( LogEvent, 2 ).
-define( LogError, 3 ).
-define( LogFatal, 4 ).

-export([
			basic_init/0,
			init/2,
			logVerbose/1,
			logInfo/1,
			logEvent/1,
			logError/1,
			logFatal/1
		]).

% export only to get correct working status.
-export( [logthread/2] ).

-vsn( p01 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Start a basic logger which output everything
%%	on the standard output.
%% @end
basic_init() ->
	init( (fun(_,Txt) -> io:put_chars(Txt) end),
			[] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Start a logger. It's a more advanced one,
%%	every logging events are passed through a
%%	given user function.
%% @end
%%
%% @spec init( Function, FuncArg ) -> Result
%% where
%%		Function = func()
%%		FuncArg = term()
%%		Result = {ok, Pid} | {error, Reason}
init( Function, FuncArg ) ->
	LogStatus = whereis( irc_serv_logger ),
	if 
		LogStatus == undefined ->
			Pid = spawn( ?MODULE, logthread, [Function, FuncArg] ),
			register( irc_serv_logger, Pid ),
			{ok, Pid};
		true -> {error, "Problem while launching logger thread"}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Log marginaly informative messages, used
%%	for debug, or to display inner information
%%	about the server.
%% @end
%%
%% @spec logVerbose( Txt ) -> nil
%% where
%%		Txt = string()
logVerbose( Txt ) ->
	logmsg( ?LogVerbose, Txt ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Log some milestones usefull for administration
%%	eg : begin/end of initialisation.
%% @end
%%
%% @spec logInfo( Txt ) -> Result
%% where
%%		Txt = string()
%%		Result = none
logInfo( Txt ) ->
	logmsg( ?LogInfo, Txt ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Log information like "a client has connected"
%%	and information needed by administration (conf
%%	reloading is a good exemple).
%% @end
%%
%% @spec logEvent( Txt ) -> nil
%% where
%%		Txt = string()
logEvent( Txt ) ->
	logmsg( ?LogEvent, Txt ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	If an event is abnormal and cause a thread
%%	crash, it should be this log level.
%% @end
%%
%% @spec logError( Txt ) -> nil
%% where
%%		Txt = string()
logError( Txt ) ->
	logmsg( ?LogError, Txt ).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Log information issued before crashing the
%%	the server, should be followed by a call to
%%	the halt() function.
%% @end
%%
%% @spec logFatal( Txt ) -> nil
%% where
%%		Txt = string()
logFatal( Txt ) ->
	logmsg( ?LogFatal, Txt ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
logmsg( ?LogVerbose, Txt ) ->
	Logger = whereis( irc_serv_logger ),
	Logger!{?LogVerbose, ": " ++ Txt};
	
logmsg( ?LogInfo, Txt ) ->
	Logger = whereis( irc_serv_logger ),
	Logger!{?LogInfo, "? " ++ Txt};
	
logmsg( ?LogEvent, Txt ) ->
	Logger = whereis( irc_serv_logger ),
	Logger!{?LogEvent,"-> " ++ Txt};
	
logmsg( ?LogError, Txt ) ->
	Logger = whereis( irc_serv_logger ),
	Logger!{?LogError, "* " ++ Txt };
	
logmsg( ?LogFatal, Txt ) ->
	Logger = whereis( irc_serv_logger ),
	Logger!{?LogFatal, "! " ++ Txt}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Transform a date/time information to
%%	a textual representation
%% @end
getTime() ->
	{ {Year, Month, Day},
		{Hour,Minute,Second} } = erlang:localtime(),
	"[" ++ integer_to_list( Hour ) ++ ":" ++
			integer_to_list( Minute ) ++ ":" ++
			integer_to_list( Second ) ++ " " ++
			integer_to_list( Month ) ++ "/" ++
			integer_to_list( Day ) ++ "/" ++
			integer_to_list( Year ) ++ "] ".

prepare( Txt ) ->
	getTime() ++ Txt ++ "\n".

logthread( Func, Args ) ->
	receive
		{?LogVerbose, What} -> Func( Args, prepare(What) );
		{?LogInfo, What} -> Func( Args, prepare(What) );
		{?LogEvent, What} -> Func( Args, prepare(What) );
		{?LogError, What} -> Func( Args, prepare(What) )
	end,
	irc_log:logthread( Func, Args ).

