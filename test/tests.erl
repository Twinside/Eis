-module( tests ).

-include_lib( "eunit/include/eunit.hrl" ).

-include( "irc_struct.hrl" ).

-export([
            cli1/0  %% fourni un client générique
            ,cli2/0 %% fourni un deuxième client générique
            ,prep_empty_server/0
            ,test_scenario/1
        ]).

make_cli( Nickname ) ->
    #client {
                send = (fun({local_test,{Nick,Pid}}, What) ->
                            Pid!(Nick ++ "_" ++ What) end)
                ,sendArgs = {local_test,{Nickname, self()}}
                ,nick = Nickname
            }.
            
cli1( ) -> make_cli( "cli1" ).
cli2( ) -> make_cli( "cli2" ).

%% @doc
%%  Prepare un serveur vide et fonctionnel pret pour
%%  envoyer des commandes
%% @end
%% @spec prep_empty_server( ) -> ServInfo
%% where
%%      ServInfo = data()
prep_empty_server( ) ->
    Cli1 = cli1(),
    Cli2 = cli2(),
    {ok,SNode,Root} = eis:dlaunch(),

    {ok, Pid1} = server_node:add_user( SNode, Cli1 ),
    {ok, Pid2} = server_node:add_user( SNode, Cli2 ),

    CliList = [ {"cli1", Pid1}, {"cli2", Pid2} ],
    {SNode,Root,CliList}.

validate_matches( _   , [] ) -> ok;
validate_matches( Data, [Match|Next] ) ->
    {ok, F} = file:open( "o.txt", [write] ),
    io:fwrite( F, "~p ~n ~p ~n", [Data, Match] ),
    file:close( F) ,
    case regexp:match( Data, Match ) of
        {match, _, _} -> validate_matches( Data, Next );
        _             -> throw( {fail, recv, Data, Match} )
    end.

wait( N ) ->
    receive
    after N -> ok
    end.

test_scenario( Tests ) ->
    {setup, local
        ,(fun () -> prep_empty_server() end) % setup
        ,(fun ({_,Root,_}) -> exit( Root, shutdown ), wait( 200 ) end) % cleanup
        ,(fun (State) -> ?_test(perform_scenario(Tests, State)) end)}.
      
perform_scenario( [], _ ) -> ok;
perform_scenario( [{Performer, norecv} | Next], State ) ->
    receive _ -> throw( {Performer, norecv} )
    after 500 -> perform_scenario( Next, State )
    end;
        
perform_scenario( [{Performer, recv, LstMatch} | Next], State ) ->
    receive
        {'EXIT', _, normal} -> perform_scenario( [{Performer, recv, LstMatch} | Next], State );
        Data -> validate_matches( Data, ["^" ++ Performer ++ "_" |LstMatch] ),
                perform_scenario( Next, State )
    after
        5000 -> throw( {performer,recv, LstMatch} )
    end;

perform_scenario( [{Performer, send, Text}|Next], State ) ->
    {_,_,NickList} = State,
    {_Nick, {_,Pid}} = lists:keysearch( Performer, 1, NickList ),
    gen_server:cast(Pid, {test, Performer, Text} ),
    perform_scenario( Next, State );
    
perform_scenario( _, _ ) -> false.
    
