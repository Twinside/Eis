-module (irc_log).

-include("log_level.hrl").

-export([
			init/1,
			logmsg/2
		]).

init( _Function ) ->
	ok.

logmsg( ?LogVerbose, Txt ) ->
	io:format(": " ++ Txt);
logmsg( ?LogInfo, Txt ) ->
	io:format("? " ++ Txt);
logmsg( ?LogEvent, Txt ) ->
	io:format("-> " ++ Txt);
logmsg( ?LogError, Txt ) ->
	io:format("* " ++ Txt);
logmsg( ?LogFatal, Txt ) ->
	io:format("! " ++ Txt).

