
%
% Containt the basic structures used by the server to
% store content.
%

% user rights
-define(IrcModeInvisible , 1).	% +i
-define(IrcModeServNotified, 2).% +s
-define(IrcModeWallMsg, 4).		% +w
-define(IrcModeServOp, 8).		% +o

% chan rights
-define(IrcModeOp, 16).			% +o
-define(IrcModeHalfop, 32).		% +h
-define(IrcModeVoice, 64).		% +v
-define(IrcModePrivate, 128).	% +p
-define(IrcModeSecret, 256).	% +s
-define(IrcModeInvite, 512).	% +i
-define(IrcModeTopic , 1024).	% +t
-define(IrcModeLocal , 2048).	% +n mode in
-define(IrcModeModerated, 4096).% +m


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
			send,		%% as function/2
			subinfo		%% as you want.
		}).
		
-record(msg,
		{
			sender,		%% as a tuple : { nick, username, host } | server
			dest,		%% as string
			ircCommand,	%% as atom or int
			params,		%% as list of string
			data		%% as string
		}).

