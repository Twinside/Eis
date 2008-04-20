-include( "irc_text.hrl" ).

% surement à bouger dans la conf,
% pour le moment je le laisse là.
-define( MAX_CHANNAME_SIZE, 18 ).
-define( MAX_NICKNAME_SIZE, 15 ).

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
            cli_listener    %% as pid()
			,nick		    %% as string
			,host = ""  	%% as string
			,username = ""	%% as string
            ,is_in = []     %% as [{string(),Pid}]
            ,rights = 0     %% global mode of the user.
			,send		    %% as function/2
			,sendArgs       %% as {local, socket()} | {virtual, Pid} | {foreign, _}
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
            ,maxchanpercli
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
            ,foreignscli% list of clients not on this server.
            
			,chans		% global list of chans on the network. {channame, managerPid}

			,maxcli
			,maxchan
		}).
