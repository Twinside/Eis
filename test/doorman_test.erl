-module( doorman_test ).

-include("irc_struct.hrl").

-include_lib( "eunit/include/eunit.hrl" ).

-record( auth,
        {
            host = ""   %% as string
            ,server     %% as pid()
            ,sock       %% as socket()
            ,nick = ""  %% as string()
            ,pass = ""  %% as string()
            ,user = ""  %% as string()
        }).

q1_test_(  ) ->
    [
	?_assert( doorman:init_state( #msg{ircCommand = 'PASS', params = ["pass"]}, undefined, #auth{} ) == {reply, continue, q2, #auth{pass = "pass"}} ),
	?_assert( doorman:q2( #msg{ircCommand = 'PASS', params = ["pass"]}, undefined, #auth{} ) == {reply, continue, q2, #auth{pass = "pass"}} ),
	?_assert( doorman:final_registration( #msg{ircCommand = 'USER', params = ["name"]}, undefined, #auth{} ) == {stop, normal, {ok, {#client{username="name", nick="", host=""}, #auth{user="name"}}}, undefined} )
    ].
