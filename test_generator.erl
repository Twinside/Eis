-export( [boucle/1] )

boucle( [Head|Tail] ) ->
    Module = list_to_atom( Head ),
    Module:test(),
    boucle( Tail );
boucle( [] ) ->
    io:format( "~n~nFIN DES TEST~n", [] ).

main( Args ) ->
    file:set_cwd("ebin"),
    boucle( Args ).

