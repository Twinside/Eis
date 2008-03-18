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

-export([
			bootstrap_balancer/0
		]).

-record( bconf,
		{
			maxress,	% maximum of client per process
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
%% Function : code to balance
%% MaxClient : Number of ressource maximum
%% per thread.
%%
start_link(Module, Function, MaxRessource) ->
	Balance = spawn(?MODULE, bootstrap_balancer,[]),
	InitialProcess = {0,
						{Module, Function, [Balance]},
						permanent, 1000, worker, [Module]},
	Conf = #bconf{ maxress= MaxRessource,
				   spec=InitialProcess,
				   curr=1 },

	case supervisor:start_link(?MODULE, InitialProcess ) of
		{ok, Pid} -> Balance!{notifysupervisor, Pid, {Conf, [InitialProcess]}},
					 {ok, Balance};

		{error, _} -> irc_log:logFatal("Cannot start LoadBlancer"),
					  {error, "Fatal"}
	end.

%%
%% Helper function to use the balancer
%%
add_ressource( BalancerPid, Rsrc ) ->
	BalancerPid!{addressource,Rsrc}.

kill_ressource( BalancerPid, Rsrc ) ->
	BalancerPid!{killressource, Rsrc}.

%
% Update the balanced process list to
% get the new ressource.
%
update_process( Count, [First | Next], New ) ->
	if First#pinfo.count == Count ->
			gen_server:cast(First#pinfo.proc, {addressource, New}),
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
	SmallestProc = (fun(Proc,Min) -> PMin = Proc#pinfo.count,
									 if Min < PMin -> Min;
										 true -> PMin end end),

	MinRes = lists:foldl( SmallestProc, ChildList, 999999 ),
	
	if MinRes >= Conf#bconf.maxress ->	% all our threads are full, launch a new one.
			Spec = setelement(1, Conf#bconf.spec, Conf#bconf.curr),
			{ok, Pid} = supervisor:start_child(SuperPid, Spec),
			gen_server:cast(Pid, {addressource, New}),
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

%
% Wait to receive the state of the balancer,
% and then launch it.
%
bootstrap_balancer() ->
	receive
		{notifysupervisor, Pid, State} -> balancer( Pid, State ); 
		_ -> irc_log:logFatal( "Wrong bootstraping of load balancer, halting"),
			halt()
	end.

%
% Thread keeping state of the process
% and there states.
% todo : add a suicide message or something.
%
balancer( SuperPid, {Conf, ChildList} ) ->
	receive
		{addressource, Rsc} ->
			load_balancer:balancer( SuperPid, ressource_adding(SuperPid, Conf, ChildList, Rsc ));

		{killressource, Rsc} ->
			_ = lists:foldl( (fun(Proc,What) -> gen_server:cast(Proc#pinfo.proc, What), What end),
							{killressource, Rsc}, ChildList ),
			load_balancer:balancer( SuperPid, {Conf, ChildList});
		
		{killedaressource, Pid} ->
			load_balancer:balancer( SuperPid, {Conf, dec_count(Pid, ChildList)} );

		_ -> error
	end.


% used by the supervisor behaviour.
init( IniChild ) ->
	ok = supervisor:check_childspecs( [IniChild] ),
	{ok,
		{						% restart a process for each dead, and stop
			{one_for_one, 1000,3600},% if more than 1 process stop in 60 seconds.
			[IniChild]
		}}.

