-module( wexpr_test ).
-include_lib( "eunit/include/eunit.hrl" ).

match_test_( ) ->
[
    ?_assert( wexpr:match("", "") )
    ,?_assert( wexpr:match( "abcdefgh", "abcdefgh" ) )
    ,?_assert( wexpr:match( "ab???fgh", "abcdefgh" ) )
    ,?_assert( wexpr:match( "a*h", "abcdefgh" ) )
    ,?_assert( wexpr:match( "\\?pouet", "?pouet" ) )
    ,?_assert( wexpr:match( "\\*pouet", "*pouet" ) )
    ,?_assert( wexpr:match( "pitbul\\?", "pitbul?" ) )
    ,?_assert( wexpr:match( "pitbul\\?", "pitbul?" ) )
    ,?_assert( wexpr:match( "a*h", "abchhhhhhhhhhhhhhhhhhhhhhhhdefgh" ) )
    ,?_assert( wexpr:match( "a*h*h*gh", "abchghhghhhhhhhhhghhhhhhghhhhghhhhhhhdefgh" ) )
    ,?_assertNot( wexpr:match("bla",    "") )
    ,?_assertNot( wexpr:match(""   , "bla") )
    ,?_assertNot( wexpr:match( "poisson\\*po", "poisson*" ) )
    ,?_assertNot( wexpr:match( "poisson\\*pi", "poisson*" ) )
    ,?_assertNot( wexpr:match( "a*h", "abcdefghd" ) )
    ,?_assertNot( wexpr:match( "a*h", "abcdefghd" ) )
    ,?_assertNot( wexpr:match( "a*hhhhhhhhhhhh", "abchhhhhhhhhhhhhhhhhhhhhhhhdefgh" ) )
    % does the next one take some time?
    ,?_assertNot( wexpr:match( "a*h****h****h*h*h*h*gh", "abchghhghhhhhhhhhghhhhhhghhhhghhhhhhhhhhhhdefghe" ) )
].

