-module(conf_loader).



% export for the gen_server
-export([
			init/1,
			start_link/1,
			handle_call/3,
			handle_cast/2,
			handle_info/2,
			terminate/2,
			code_change/3,
			getElement/1
		]).

loadConf(FileName) ->
	{ok, Device} = file:open(FileName, [read]),
	loadConf(Device, []).
	
loadConf(Device, Conf) ->
	case io:get_line(Device, "") of
		eof  -> 
			file:close(Device), 
			Conf;
		Line -> 
			%% Supprime les " ", \n,le "=" et les "
			loadConf(Device, [string:tokens(
								string:strip(
									string:strip(
										string:strip(Line, both, $\n),
										both, $=),
									both, $"),
								" =")|Conf])
	end.

getElement( [ [Name|Val] | Queue ], Seek ) ->
	if Name == Seek ->
		Val;
	true -> 
		getElement (Queue, Seek)
	end;

getElement( [], _Seek) ->
	not_found.

getElement( Name ) ->
	case whereis(conf_loader) of
		undefined ->
			error;
		Pid ->
			gen_server:call( Pid, {get, Name} )
	end.


start_link( FileName ) ->
	gen_server:start_link( ?MODULE, FileName, [] ).

init( FileName ) ->
	register( conf_loader, self() ),
	{ok, loadConf( FileName )}.

handle_call( {get, Name}, _From, Conf ) ->
	Reply = getElement(Conf, Name),	
	{reply, Reply, Conf};

handle_call( {set, _Name, _Value }, _From, _Conf ) ->
	NewState = undefined,
	{noreply, NewState};

handle_call( {reload, FileName}, _From, _Conf ) ->
	NewState = loadConf(FileName),
	{noreply, NewState};

handle_call( {save, _FileName}, _From, Conf ) ->
	%% Sauver la conf
	{noreply, Conf}.
     
handle_cast( _Command, State ) ->
	{noreply, State}.
	
handle_info(_What, State) ->
	{noreply, State}.

terminate(_Reason,State) ->
	%% Sauver la conf	
	{ok, State}.

code_change(_OldVsn, State,_Extra) ->
	{ok, State}.
