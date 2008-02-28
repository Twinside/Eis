
%
% Containt the basic structures used by the server to
% store content.
%

-record(chan,
		{
			channame,	%% as string
			userlimit,	%% as int
			banlist,	%% as string list
			topic,		%% as string
			mode		%% as int interpreted as flag.
		}).

-record(client,
		{
			nick,		%% as string
			host,		%% as string
			username,	%% as string
			send		%% as function/1
		}).
		
-record(msg,
		{
			sender,		%% as a tuple : { nick, username, host } | server
			dest,		%% as string
			ircCommand,	%% as atom or int
			params,		%% as list of string
			data		%% as string
		}).
