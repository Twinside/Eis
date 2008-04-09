%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	Module in charge of the dynamic balancing of 
%%	different ressources. can start a given number
%%	of thread to handle the charge.
%% @end
-module (load_balancer).

-behaviour( supervisor ).
-behaviour( gen_server ).

% export for the behaviour.
-export([init/1]).

% export for the gen_server
-export([
			%init/1,
			handle_call/3,
			handle_cast/2,
			handle_info/2,
			terminate/2,
			code_change/3
		]).

-export([
			start_link/3		% to launch a blancer.
			,add_ressource/2	% helper to add a ressource
			,kill_ressource/2	% helper to add a ressource.
		]).

-record( bconf,
		{
			maxress,	% maximum of client per process
			spec,		% general process specificatioun
			suppid,		% pid of the supervisor
			count = 1,		% current process count.
			children = []	% list of... children...
		}).
		
-record( pinfo,
		{
			count = 0,	% as integer
			proc,	% as Pid
			spec	% as childspec (supervisor)
		}).

-vsn( p01 ).

%% @doc
%% Call to start the balancer.
%%	<p>
%%	Module/Function is what to start, MaxRessource is the upper
%%	bound of balanced ressources.
%%	</p>
%% @end
%% @spec start_link( Module, Function, MaxRessource ) -> Result
%% where
%%		Module = atom()
%%		Function = atom()
%%		MaxRessource = integer()
start_link(Module, Function, {MaxRessource, Referer}) ->
	{ok, Balance} = gen_server:start_link( ?MODULE, [ {serverinit, unit} ], [] ),
	InitialProcess = {0,
						{
                            Module,
                            Function,
                            [ {Balance, Referer} ]
                         },
						permanent, 1000, worker, [Module]},
	case supervisor:start_link(?MODULE, InitialProcess ) of
		{ok, Pid} -> Conf = #bconf{ maxress= MaxRessource,
									suppid = Pid,
								   spec=InitialProcess },
					 gen_server:cast( Balance, Conf ),
					 {ok, Balance};

		{error, _} -> irc_log:logFatal("Cannot start LoadBlancer"),
					  {error, "Fatal"}            
	end.

%% @doc
%% 	add a ressource to a balance.
%%	return the pid of the processing which
%%	is managing the ressource
%% @end
%% @spec add_ressource( BalancerPid, Rsrc ) -> Result
%% where
%%		BalancerPid = pid()
%%		Rsrc = term()
%%		Result = {ok, Pid} | {error, Reason}
add_ressource( BalancerPid, Rsrc ) ->
	gen_server:call( BalancerPid, {addressource, Rsrc } ).

%% @doc
%%	Remove a ressource from the balance. 
%% @end
%% @spec kill_ressource( BalancerPid, Rsrc ) -> term()
%% where
%%		BalancerPid	= pid()
%%		Rsrc = term()
kill_ressource( BalancerPid, Rsrc ) ->
	gen_server:cast( BalancerPid, {killressource, Rsrc} ).

%
% Update the balanced process list to
% get the new ressource.
%
update_process( Count, Prev, [First | Next], New, State ) 
	when First#pinfo.count == Count->

	gen_server:cast(First#pinfo.proc, {addressource, New}),
	NewPinfo = First#pinfo{ count = Count + 1 },
	NeoState = State#bconf{ children =  Prev ++ [NewPinfo |Next] },
	{NewPinfo#pinfo.proc, NeoState};

update_process( Count, Prev, [First|Next], New, State ) ->
	update_process(Count, [First| Prev], Next, New, State).
	
%
% In charge of adding a ressource.
% SuperPid : pid of the supervisor
% Conf : configuration state of the balancer.
% ChildList : list of balanced process
% New : ressource to add.
ressource_adding( State, Rsrc ) ->
	SmallestProc = (fun(Proc,Min) -> PMin = Proc#pinfo.count,
									 if Min < PMin -> Min;
										 true -> PMin end end),

	MinRes = lists:foldl( SmallestProc, 16#7FFFFFFF, State#bconf.children ),
	
	if MinRes >= State#bconf.maxress ->	% all our threads are full, launch a new one.
			Spec = setelement(1, State#bconf.spec, erlang:make_ref() ),
			{ok, Pid} = supervisor:start_child(State#bconf.suppid, Spec),
			gen_server:cast(Pid, {addressource, Rsrc}),
			NewProcess = #pinfo{count=1, proc=Pid, spec=Spec},
			NewConf = State#bconf{ children = [NewProcess] ++ State#bconf.children,
									count = State#bconf.count + 1 },
			{Pid, NewConf};
			
		true -> update_process( MinRes, [], State#bconf.children, Rsrc, State )
	end.


% decrement ressource count of a process.
dec_count( _Pid, _Prev, [], State ) ->
	irc_log:logError( "Ressource killed by a non-balance" ),
	State;

dec_count( Pid, Prev, [P | Next], State )
	when P#pinfo.proc == Pid ->

	Upd = P#pinfo{ count = P#pinfo.count - 1 },
	State#bconf{ children =	Prev ++ [Upd | Next] };

dec_count( Pid, Prev, [P | Next], State ) ->
	dec_count( Pid, [P|Prev], Next, State ).

%% @doc
%%  Thread keeping state of the process
%%  and there states.
%% @end
%% @todo : add a suicide message or something.
%% @hidden
handle_call( {addressource, Rsc}, _From, State ) ->
	{Assigned, NeoState} = ressource_adding( State, Rsc ),
	{reply, Assigned, NeoState};

handle_call( _, _, State ) ->
	irc_log:logError( "Unrecognized call to load balancer" ),
	{noreply, State}.

handle_cast( {killressource, Rsc}, State ) ->
	NeoState = lists:foldl( (fun(Proc,What) -> gen_server:cast(Proc#pinfo.proc, What), What end),
					{killressource, Rsc}, State#bconf.children ),
	{noreply, NeoState};

% Used to set the state of ther server at
% the begining of it's lifetime.
handle_cast( State, void ) ->
	{noreply, State};

handle_cast( {killedaressource, Pid}, State ) ->
	{noreply, dec_count( Pid, [], State#bconf.children, State )};

handle_cast( _, State ) ->
	irc_log:logError( "Unrecognized balancer call" ),
	{noreply, State}.

%% used by the supervisor and gen_server behaviour.
%% @hidden
init( [{serverinit, unit}] ) ->
	{ok, void};

init( IniChild ) ->
	ok = supervisor:check_childspecs( [IniChild] ),
	{ok,
		{						% restart a process for each dead, and stop
			{one_for_one, 1000,3600},% if more than 1 process stop in 60 seconds.
			[]
		}}.

handle_info( _, _ ) -> undefined.
terminate( _, _ ) -> undefined.
code_change( _, _, _) -> undefined.

