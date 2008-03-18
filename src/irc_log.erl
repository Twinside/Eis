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

basic_init() ->
	init( (fun(_,Txt) -> io:put_chars(Txt) end),
			[] ).

init( Function, FuncArg ) ->
	LogStatus = whereis( irc_serv_logger ),
	if 
		LogStatus == undefined ->
			Pid = spawn( ?MODULE, logthread, [Function, FuncArg] ),
			register( irc_serv_logger, Pid );
		true -> ok
	end,
	ok.

logVerbose( Txt ) ->
	logmsg( ?LogVerbose, Txt ).

logInfo( Txt ) ->
	logmsg( ?LogInfo, Txt ).

logEvent( Txt ) ->
	logmsg( ?LogEvent, Txt ).

logError( Txt ) ->
	logmsg( ?LogError, Txt ).
	
logFatal( Txt ) ->
	logmsg( ?LogFatal, Txt ).
	
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

