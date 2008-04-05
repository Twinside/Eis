-export( [boucle/1] )

boucle( [Head|Tail] ) ->
    Head:test(),
    boucle( Tail );
boucle( [] ) ->
    io:format( "~n~nFIN DES TEST~n", [] ).

main( Args ) ->
    file:set_cwd("ebin"),
    boucle( Args ).

