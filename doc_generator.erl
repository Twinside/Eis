-include( "edoc.hrl" ).

main( Args ) ->
	edoc:run([],
			Args,
			[	{dir, "doc"},
				{new, true},
				{source_path, ["src"]} ]
			).

