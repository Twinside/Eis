%% @doc
%% <p>
%%  This module is in charge of notifying
%%  a client listener to perform a ping send.
%%  it also contain the code to perform
%%  the ping send and pong handling and,
%%  finaly, cleanup
%% </p><p>
%%  The module rely on the com_quit code
%%  to perform effectively the cleanup.
%% </p>
%% @end
-module( pinger ).

-include( "irc_struct.hrl" ).

-vsn( p01 ).


-export([
            start_link/0
            ,perform_client/3
            ,clean_unponged/1
            ,send_ping/1
            ,pinger/1
        ]).

-record( pingst,
        {
            notified        %% as pid()
            ,ping_cycle     %% as int()
            ,pong_timeout   %% as int()
        }).

load_conf( NotifPid ) ->
    #pingst {
        notified = NotifPid
        ,ping_cycle = conf_loader:get_int_conf( ping_cycle )
        ,pong_timeout = conf_loader:get_int_conf( pong_timeout )
    }.

%% @doc
%%  Start a timer which is in charge of
%%  notifying a client listener when to
%%  ping his client and cleanup them.
%% @end
%% @spec start_link() -> Rez
%% where
%%      Rez = {ok, pid()}
start_link() ->
    State = load_conf( self() ),
    {ok, spawn( ?MODULE, pinger, [State] )}.

wait( Time ) ->
    receive
    after Time -> none end.

%% @doc
%%  Infinite loop of the timer.
%% @end
%% @hidden
pinger( State ) ->
    wait( State#pingst.ping_cycle ),
    gen_server:cast( State#pingst.notified, ping ),
    wait( State#pingst.pong_timeout ),
    gen_server:cast( State#pingst.notified, pong ),
    pinger( State ).

%% @doc
%%  Handle the pong message.
%% @end
perform_client( _Msg, Cli, ClientState ) ->
    NeoCli = Cli#client{ pinged = true },
    ets:insert( ClientState#listener.bynick,
                {NeoCli#client.nick, NeoCli} ),
    ClientState.

%% @doc
%%  To call when the listener need to clean
%%  all the client which doesn't respond to
%%  the PING message
%% @end
%% @spec clean_unponged( ClientState ) -> NewState
%% where
%%      ClientState = listener()
%%      NewState = listener()
clean_unponged( ClientState ) ->
    Func = (fun( {_Key, Cli}, Lst) ->
                if Cli#client.pinged -> Lst;
                   true -> [Cli | Lst]
                 end
            end),
            
    CleanMsg = #msg { ircCommand = 'QUIT',
                      data = ?PING_TIMEOUT_TXT },
    Cleaner = (fun (Cli, State) ->
                    com_quit:perform_client( CleanMsg, Cli, State )
                end),
    ToClean = ets:foldl( Func, [], ClientState#listener.bynick ),
    lists:foldl( Cleaner, ClientState, ToClean )
    .

%% @doc
%%  Function to call to send a PING message to all
%%  the users of a client listener.
%% @end
%% @spec send_ping( ClientState ) -> NeoState
%% where
%%      ClientState = listener()
%%      NeoState = listener()
send_ping( ClientState ) ->
    Ping = "PING :" ++ ClientState#listener.server_host ++ "\r\n",
    Func = (fun({Key, Cli}, Updated) ->
                (Cli#client.send)(Cli#client.sendArgs, Ping),
                [{Key, Cli#client{ pinged = false }} | Updated ]
            end),
    New = ets:foldl( Func, [], ClientState#listener.bynick ),
    ets:insert( ClientState#listener.bynick, New ),
    ClientState.

