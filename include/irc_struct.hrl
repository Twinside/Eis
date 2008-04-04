
%
% Containt the basic structures used by the server to
% store content.
%

% Defining IRC error codes for use when
% replying with a numerical code :)
-define( ERR_NEEDMOREPARAMS, "461 :Error need more parameters" ).
-define( ERR_ALREADYREGISTERED, 462 ).
-define( ERR_BADCHANNELKEY, "475 :Error bad chanel key" ).
-define( ERR_CHANNELISFULL, "471 :Error channel is full" ).
-define( ERR_NOSUCHCHANNEL, "403 :Error no such channel" ).
-define( RPL_WELCOME, 001 ).
-define( RPL_YOURHOST, 002 ).
-define( RPL_CREATED, 003 ).
-define( RPL_MYINFO, 004 ).
-define( RPL_BOUNCE, 005 ).

-define( RPL_MOTDSTART, 375 ).
-define( RPL_MOTD, 372 ).
-define( RPL_ENDOFMOTD, 476 ).

% surement à bouger dans la conf,
% pour le moment je le laisse là.
-define( MAX_CHANNAME_SIZE, 18 ).
-define( MAX_NICKNAME_SIZE, 15 ).

-define( CHAN_INDEX_USERCOUNT, 3 ).
-record(chan,
		{
			channame	    %% as string
			,userlimit = 0  %% as int
            ,usercount = 0  %% as int
            ,password = ""  %% as string
			,banlist = []	%% as string list
			,topic = ""		%% as string
			,mode = 0	    %% as int interpreted as flag.
            ,userlist       %% List of locals user to broadcast.
            ,foreignusers   %%
		}).

-record(client,
		{
			nick		%% as string
			,host		%% as string
			,username	%% as string
			,send		%% as function/2
			,sendArgs    %% as socket() | {virtual, Pid}
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
            ,server_host %% as string
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
