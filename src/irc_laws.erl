%% @doc
%%	Module dedicated to the rights management.
%%	All the right management go through this module,
%%	everything use macro to avoid many redundant code.
%% @end
-module( irc_laws ).

-include( "irc_struct.hrl" ).

-define( MINVISIBLE, (1 bsl 0) ).
-define( MNOTIFIED,  (1 bsl 1) ).
-define( MWALLOP,    (1 bsl 2) ).
-define( MSERVOP,    (1 bsl 3) ).

-define( MCHANOP,    (1 bsl 4) ).
-define( MPRIVATE,   (1 bsl 5) ).
-define( MSECRET,    (1 bsl 6) ).
-define( MINVITE,    (1 bsl 7) ).
-define( MTOPIC,     (1 bsl 8) ).
-define( MMODERATED, (1 bsl 9) ).
-define( MFROMINSIDE,(1 bsl 10) ).
-define( MLIMITED,   (1 bsl 11) ).
-define( MKEY,       (1 bsl 12) ).
-define( MVOICED,	 (1 bsl 13) ).
-define( MHALFED,	 (1 bsl 14) ).

-define( MODE_LAST,	 15 ).

-define( INSIDE,     (1 bsl 15) ).
-define( MBAN,       (1 bsl 16) ).

-define( NOT_MINVISIBLE,	(bnot ?MINVISIBLE)).
-define( NOT_MNOTIFIED,		(bnot ?MNOTIFIED)).
-define( NOT_MWALLOP,		(bnot ?MWALLOP)).
-define( NOT_MSERVOP,		(bnot ?MSERVOP)).

-define( NOT_MCHANOP,		(bnot ?MCHANOP)).
-define( NOT_MPRIVATE,		(bnot ?MPRIVATE)).
-define( NOT_MSECRET,		(bnot ?MSECRET)).
-define( NOT_MINVITE,		(bnot ?MINVITE)).
-define( NOT_MTOPIC,		(bnot ?MTOPIC)).
-define( NOT_MMODERATED,	(bnot ?MMODERATED)).
-define( NOT_MFROMINSIDE,	(bnot ?MFROMINSIDE)).
-define( NOT_MLIMITED,		(bnot ?MLIMITED)).
-define( NOT_MKEY,			(bnot ?MKEY)).

-define( ANYUSER, 0 ).
-define( ANYMODE, ((1 bsl ?MODE_LAST) - 1)).

-export([
			check_chanlaw/3
			,check_granting/2
%			,mode_translate/1
            ,is_chan_passworded/1
            ,is_chan_inviteonly/1
            ,is_chan_limited/1
            ,is_chan_private/1
            ,is_chan_inmsgonly/1
            ,choose_welcome_right/1
            ,chan_mode_to_string/1
            ,global_mode_to_string/1

            ,string_to_globalmode/1
            ,string_to_usermode/1
            ,prefix_nick/2
		]).

-define( ac(A,V), mc( S, [A|_] ) -> {S,V} ).
-define( au(A,V), mu( S, [A|_] ) -> {S,V} ).

chan_modes( ) ->
[
     { $v, ?MVOICED  }
    ,{ $o, ?MCHANOP  }
    ,{ $h, ?MHALFED  }
    ,{ $p, ?MPRIVATE }
    ,{ $s, ?MSECRET  }
    ,{ $i, ?MINVITE  }
    ,{ $n, ?MFROMINSIDE }
    ,{ $t, ?MTOPIC }
    ,{ $m, ?MMODERATED }
    ,{ $l, ?MLIMITED }
    ,{ $k, ?MKEY }
    ,{ $b, ?MBAN }
].

global_modes( ) ->
[
     { $i, ?MINVISIBLE }
    ,{ $s, ?MNOTIFIED }
    ,{ $o, ?MSERVOP }
    ,{ $w, ?MWALLOP }
].

%% @doc
%%  Return a nick with the good prefix to
%%  send in names/who replies.
%% @end
%% @spec prefix_nick( Nick, Usermode ) -> Result
%% where
%%      Nick = string()
%%      Usermode = int()
%%      Result = string()
prefix_nick( St, M )
    when M band ?MCHANOP /= 0 -> [$@ | St];
prefix_nick( St, M )
    when M band ?MHALFED /= 0 -> [$% | St];
prefix_nick( St, M )
    when M band ?MVOICED /= 0 -> [$+ | St];
prefix_nick( St, _ ) -> St.

%% @doc
%%  Convert an IRC string received
%%  to an irc mode usable by the
%%  system. Cover the global modes.
%% @end
%% @spec string_to_globalmode( Str ) -> Result
%% where
%%      Str = string()
%%      Result = {Side, Mode}
%%              | {unknwon, Side, Mode}
%%      Side = plus | minus
%%      Mode = int()
string_to_globalmode( Str ) ->
    string_to_mode(global_modes() , $+, Str )
    .

%% @doc
%%  Convert an IRC string received
%%  to an irc mode usable by the
%%  system. Cover modes for user in the
%%  channel and the channel itself
%% @end
%% @spec string_to_usermode( Str ) -> Result
%% where
%%      Str = string()
%%      Result = {Side, Mode}
%%              | {unknwon, Side, Mode}
%%      Side = plus | minus
%%      Mode = int()
string_to_usermode( Str ) ->
    string_to_mode(chan_modes() , $+, Str )
    .

string_to_mode( Lst, _, [$+|St] ) ->
    string_to_mode( Lst, $+, St );
string_to_mode( Lst, _, [$-|St] ) ->
    string_to_mode( Lst, $-, St );
string_to_mode( Lst, Side, [C|_] ) ->
    case lists:keysearch( C , 1, Lst ) of
        {value, {_,M}} -> {Side, M};
        _              -> {unknown, Side, C}
    end
    .

%% @doc
%%  Convert an user right on chan or a
%%  chan right to a string.
%% @end
%% @spec chan_mode_to_string( Mode ) -> Result
%% where
%%      Mode = int()
%%      Result = string()
chan_mode_to_string( Mode ) ->
    [$+ | mode_to_string( Mode
                        ,chan_modes()
                        ,?MODE_LAST - 1) ]
    .

%% @doc
%% convert a global user mode to a string to
%% be sent on the network.
%% @end
%% @spec global_mode_to_string( Mode ) -> Result
%% where
%%      Mode = int()
%%      Result = string()
global_mode_to_string( Mode ) ->
    [$+ | mode_to_string( Mode
                        ,global_modes()
                        ,?MODE_LAST - 1) ]
    .

mode_to_string( _, _, 0 ) -> [];
mode_to_string( Mode, Lassoc, I ) ->
    Val = 1 bsl I,
    if Val band Mode ->
                {value, {Ch,_}} = lists:keysearch( (1 bsl I), 2, Lassoc ),
                [Ch | mode_to_string(Mode, Lassoc, I - 1)];

       true -> mode_to_string( Mode, Lassoc, I - 1 )
    end
    .

%% @doc
%%  Tell if a chan is private (+p).
%% @end
%% @spec is_chan_private( Chan ) -> bool
is_chan_private( Chan ) ->
    (Chan#chan.mode band ?MPRIVATE) /= 0.

%% @doc
%%  Tell if chan accept only message from
%%  inside.
%%  Only message of users within the
%%  chan are allowed in private chans.
%% @end
is_chan_inmsgonly( Chan ) ->
    (Chan#chan.mode band ?MFROMINSIDE) /= 0.
    
%% @doc
%%  Tell if a chan got the password protection
%%  enabled (+k)
%% @end
%% @spec is_chan_passworded( Chan ) -> bool
%% where
%%      Chan = chan()
is_chan_passworded( Chan ) ->
    (Chan#chan.mode band ?MKEY) /= 0.
    
%% @doc
%%  Tell if a chan got the password protection
%%  enabled (+i)
%% @end
%% @spec is_chan_inviteonly( Chan ) -> bool
%% where
%%      Chan = chan()
is_chan_inviteonly( Chan ) ->
    (Chan#chan.mode band ?MINVITE) /= 0.

%% @doc
%%  Tell if a chan got the size limit protection.
%%  enabled (+L)
%% @end
%% @spec is_chan_limited( Chan ) -> bool
%% where
%%      Chan = chan()
is_chan_limited( Chan ) ->
    (Chan#chan.mode band ?MLIMITED) /=0.

%% @doc
%%  Give the initial user right in function
%%  of the number of user already in the chan.
%% @end
%% @spec choose_welcome_right( N ) -> Result
%% where
%%      N = int()
%%      Result = rights()
choose_welcome_right( 0 ) -> ?MCHANOP bor ?INSIDE;
choose_welcome_right( _ ) -> ?INSIDE.

imply( A, B ) -> (not (A /= 0)) or (B /= 0).
is( A, B ) -> (A band B) /= 0.

%% @doc
%%  Check if an user can do the action in
%%	a chan. Check the user rights against
%%	the chan rights and IRC command to
%%	determine the authorization.
%% @end
%% @spec check_chanlaw( IrcCommand, UserRight, ChanRight ) -> bool
%% where
%%		IrcCommand = atom()
%%		UserRight = int
%%		ChanRight = int
check_chanlaw( 'PRIVMSG', Ur, Cr ) ->
    imply( Cr band ?MMODERATED, Ur band ?MVOICED ) and
    imply( Cr band ?MFROMINSIDE, Ur band ?INSIDE );

check_chanlaw( 'TOPIC', Ur, Cr ) ->
    imply( Cr band ?MTOPIC,
           Ur band (?MCHANOP bor ?MHALFED));

check_chanlaw( 'KICK', Ur, _Cr) ->
    is( Ur, ?MCHANOP bor ?MHALFED );

check_chanlaw( _Command, _User, _Chan) -> false.

%% @doc
%%  Check if a given user can grant another user/chan
%%  given it's actual right in the channel.
%% @end
%% @spec check_granting( Permission, Userright ) -> bool()
%% where
%%      Permission = int()
%%      Userright = int()
check_granting( ?MSECRET, Ur ) -> is( Ur, ?MCHANOP );
check_granting( ?MTOPIC, Ur ) -> is( Ur, ?MCHANOP );
check_granting( ?MKEY  , Ur ) -> is( Ur, ?MCHANOP );
check_granting( ?MMODERATED, Ur ) -> is( Ur, ?MCHANOP );
check_granting( ?MFROMINSIDE, Ur ) -> is( Ur, ?MCHANOP );
check_granting( ?MLIMITED, Ur ) -> is( Ur, ?MCHANOP );
check_granting( ?MPRIVATE, Ur ) -> is( Ur, ?MCHANOP );
check_granting( ?MINVITE, Ur ) -> is( Ur, ?MCHANOP );

check_granting( ?MBAN, Ur ) -> is( Ur, ?MCHANOP bor ?MHALFED );

check_granting( ?MCHANOP, Ur ) -> is( Ur, ?MCHANOP );
check_granting( ?MHALFED, Ur ) -> is( Ur, ?MCHANOP );
check_granting( ?MVOICED, Ur ) -> is( Ur, ?MCHANOP bor ?MHALFED );

check_granting( _Mode, _From ) -> false.

