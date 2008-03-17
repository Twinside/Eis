-module (load_balancer).

-include( "log_level.hrl" ).

-behaviour(supervisor).

% export for the behaviour.
-export([init/1]).


-export([
			start_link/3,		% to launch a blancer.
			add_ressource/2,	% helper to add a ressource
			kill_ressource/2,	% helper to add a ressource.
			balancer/2
		]).


-record( bconf,
		{
			maxcli,	% maximum of client per process
			spec,	% general process specification
			curr	% current process count.
		}).
		
-record( pinfo,
		{
			count,	% as integer
			proc,	% as Pid
			spec	% as childspec (supervisor)
		}).

-vsn( p01 ).

%%
%% Call to start the balancer.
%% Module, module of code to balance
%% Initiator : code to balance
%% MaxClient : Number of ressource maximum
%% per thread.
%%
start_link(Module, Initiator, MaxClient) ->
	InitialProcess = {0, {Module, Initiator,[]},
						transient, brutal_kill, worker, [gen_server]},
	case supervisor:start_link(?MODULE, {Module,Initiator} ) of
		{ok, Pid} -> Conf = #bconf {maxcli= MaxClient,
									spec=InitialProcess,
									curr=1}, 
					{ok, spawn(?MODULE, balancer, [Pid, {Conf, [InitialProcess]} ])};

		{error, _} -> irc_log:logmsg(?LogFatal,"Cannot start LoadBlancer"),
						{error, "Fatal"}
	end.

%%
%% Helper function to use the balancer
%%
add_ressource( BalancerPid, Rsrc ) ->
	BalancerPid!{addressource,Rsrc}.

kill_ressource( BalancerPid, Rsrc ) ->
	BalancerPid!{killressource, Rsrc}.
	
% return the smallest process in term of managedcount for the
smallest_process( Proc, Min ) ->
	PMin = Proc#pinfo.count,
	if Min < PMin -> Min;
			true -> PMin
	end.

%
% Update the balanced process list to
% get the new ressource.
%
update_process( Count, [First | Next], New ) ->
	if First#pinfo.count == Count ->
			First#pinfo.proc!{addressource, New},
			[#pinfo{count = Count + 1,
					proc = First#pinfo.proc,
					spec = First#pinfo.spec} |Next];
		true -> [First | update_process(Count, Next, New)]
	end.

%
% In charge of adding a ressource.
% SuperPid : pid of the supervisor
% Conf : configuration state of the balancer.
% ChildList : list of balanced process
% New : ressource to add.
ressource_adding( SuperPid, Conf, ChildList, New ) ->
	MinRes = lists:foldl( smallest_process, ChildList, 999999 ),
	if MinRes >= Conf#bconf.maxcli ->	% all our threads are full, launch a new one.
			Spec = setelement(1, Conf#bconf.spec, Conf#bconf.curr),
			{ok, Pid} = supervisor:start_child(SuperPid, Spec),
			Pid!{addressource, New},
			NewProcess = #pinfo{count=1, proc=Pid, spec=Spec},
			NewConf = setelement(3, Conf, Conf#bconf.curr + 1),
			[First | Next] = ChildList,			% used to recombine the new list.
			{NewConf, [NewProcess,First|Next]};
		true -> {Conf, update_process( MinRes, ChildList, New )}
	end.


% decrement ressource count of a process.
dec_count( _Pid, [] ) -> [];
dec_count( Pid, [P | Next] ) ->
	if P#pinfo.proc == Pid -> N = #pinfo { count = P#pinfo.count - 1,
											proc = Pid,
											spec = P#pinfo.spec },
							[N | Next];
		true -> [P | dec_count( Pid, Next )]
	end.

% helper function used to send a message
% to all the managed process 
broadcast( Proc, What ) ->
	Proc#pinfo.proc!What,
	What.
%
% Thread keeping state of the process
% and there states.
% todo : add a suicide message or something.
%
balancer( SuperPid, {Conf, ChildList} ) ->
	receive
		{addressource, Rsc} ->
			balancer( SuperPid, ressource_adding(SuperPid, Conf, ChildList, Rsc ));

		{killressource, Rsc} ->
			_ = lists:foldl( broadcast, {killressource, Rsc}, ChildList ),
			balancer( SuperPid, {Conf, ChildList});
		
		{killedaressource, Pid} -> {Conf, dec_count(Pid, ChildList)};
		_ -> error
	end.


% used by the supervisor behaviour.
init( IniChild ) ->
	{ok,
		{						% restart a process for each dead, and stop
			{one_for_one, 1,60},% if more than 1 process stop in 60 seconds.
			IniChild
		}}.

