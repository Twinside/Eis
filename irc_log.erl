-module (irc_log).

-include("log_level.hrl").

-export([
			init/1,
			logmsg/2
		]).

init( _Function ) ->
	ok.

logmsg( ?LogVerbose, _Txt ) ->
	ok;
logmsg( ?LogInfo, _Txt ) ->
	ok;
logmsg( ?LogEvent, _Txt ) ->
	ok;
logmsg( ?LogError, _Txt ) ->
	ok;
logmsg( ?LogFatal, _Txt ) ->
	ok.

