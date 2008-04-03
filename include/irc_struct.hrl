
%
% Containt the basic structures used by the server to
% store content.
%

% Defining IRC error codes for use when
% replying with a numerical code :)
-define( ERR_NEEDMOREPARAMS, "461 :Error need more parameters" ).
-define( ERR_ALREADYREGISTERED, 462 ).

-define( RPL_WELCOME, 001 ).
-define( RPL_YOURHOST, 002 ).
-define( RPL_CREATED, 003 ).
-define( RPL_MYINFO, 004 ).
-define( RPL_BOUNCE, 005 ).

-define( RPL_MOTDSTART, 375 ).
-define( RPL_MOTD, 372 ).
-define( RPL_ENDOFMOTD, 476 ).


-record(chan,
		{
			manager,	%% as pid()
			channame,	%% as string
			userlimit,	%% as int
			banlist,	%% as string list
			topic,		%% as string
			mode		%% as int interpreted as flag.
		}).

-record(client,
		{
			nick		%% as string
			,host		%% as string
			,username	%% as string
			,send		%% as function/2
			,sendArgs    %% as socket() | {virtual, Pid}
			,subinfo		%% as you want.
		}).
		
-record(msg,
		{
			sender		%% as a tuple : { nick, username, host } | server
			,ircCommand	%% as atom or int
			,params		%% as list of string
			,data		%% as string
		}).


-record( listener,
		{
			server_host %% string
			,supervisor	%% balance
			,servernode	%% pid of the server node.
			,bynick
			,bysock
		}).

-record( cmanager,
		{
			bal			%% PID of balance
			,serv		%% PID of servernode.
			,byname		%% association table with chan name in key
		}).

-record( srvs,
		{
			supervisor
			,clibal		% load balancer for client
			,chanbal	% load balancer for channels
			,clients	% global list of clients connected to this server.
			,chans		% global list of chans on the network. {channame, managerPid}

			,maxcli
			,maxchanpercli	% fuck.
		} ).
