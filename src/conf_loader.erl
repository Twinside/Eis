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
			get_conf/1,
            get_int_conf/1
		]).

%% @doc
%%  Recherche la valeur d'une configuration
%% @end
%% @spec get_conf( Name ) -> Result
%% where
%%      Name = atom() | string()
%%      Result = string() | error
get_conf( Name ) when is_atom( Name ) ->
    get_conf( atom_to_list( Name ));
get_conf( Name ) ->
	case whereis(conf_loader) of
		undefined ->
			error;
		Pid ->
			gen_server:call( Pid, {get, Name} )
	end.

%% @doc
%%  Cherche une configuration et tente de
%%  la convertir en entier.
%% @end
%% @spec get_int_conf( Name ) -> Result
%% where
%%      Name = string() | atom()
%%      Result = int() | error
get_int_conf( Name ) ->
    case get_conf( Name ) of
        error -> error;
        Rez -> list_to_integer( Rez )
    end.

loadConf(FileName) ->
	{ok, Device} = file:open(FileName, [read]),
	loadConf(Device, []).

loadConf(Device, Conf) ->
	case io:get_line(Device, "") of
		eof  ->
			file:close(Device),
			Conf;
		Line ->
			case parse_line( Line ) of
				[Key, Val|_] -> loadConf( Device, [{Key,Val} | Conf] );
				_ -> loadConf( Device, Conf )
			end
			%% Supprime les " ", \n,le "=" et les "
	end.

parse_line( [$#  | _] ) -> none;
parse_line( [$\r | _] ) -> none;
parse_line( [$\n | _] ) -> none;
parse_line( Line ) ->
	Endcleaned = string:strip(Line, both, $\n),
	Cuted = string:strip( Endcleaned, both, $  ),
	Dequoted = string:strip( Cuted, both, $"), %"
	string:tokens( Dequoted, "=").

%% @hidden
getElement( [ {Name, Val} | Queue ], Seek ) ->
	if Name == Seek ->
		Val;
	true ->
		getElement (Queue, Seek)
	end;

getElement( [], _Seek) ->
	not_found.


%% @doc
%% Creer un processus pour le conf_loader et charge la configuration
%% @end
start_link( FileName ) ->
	gen_server:start_link( ?MODULE, FileName, [] ).

%% @hidden
init( FileName ) ->
	register( conf_loader, self() ),
	{ok, loadConf( FileName )}.

%% @hidden
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
%% @hidden
handle_cast( _Command, State ) ->
	{noreply, State}.
%% @hidden
handle_info(_What, State) ->
	{noreply, State}.
%% @hidden
terminate(Reason,_State) ->
    unregister( conf_loader ),
	%% Sauver la conf
    Reason.

%% @hidden
code_change(_OldVsn, State,_Extra) ->
	{ok, State}.
