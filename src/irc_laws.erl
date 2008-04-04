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

-define( MODE_LAST,	 (1 bsl 15) ).

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
-define( ANYMODE, (?MODE_LAST - 1)).

-export([
			check_chanlaw/3
			,check_granting/2
%			,mode_translate/1
            ,is_chan_passworded/1
            ,is_chan_inviteonly/1
            ,is_chan_limited/1
            ,choose_welcome_right/1
		]).

-define( allow_chan( Command, UserRight, ChanRight ),
		check_chanlaw( Command, User, Chan )
			when ((UserRight band User) == UserRight) and
				 ((ChanRight band Chan) == Chan) -> true
		).

-define( allow_grant( Mode, From ),
		check_granting( Mode, FromUser )
			when (From band FromUser) /= 0  -> true
		).

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
%% @spec is_chan_passworded( Chan ) -> bool
%% where
%%      Chan = chan()
is_chan_inviteonly( Chan ) ->
    (Chan#chan.mode band ?MINVITE) /= 0.

%% @doc
%%  Tell if a chan got the size limit protection.
%%  enabled (+L)
%% @end
%% @spec is_chan_passworded( Chan ) -> bool
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
choose_welcome_right( 0 ) -> ?MCHANOP;
choose_welcome_right( _ ) -> 0.

% % We refuse everything, unless the rules is defined below.

?allow_chan( 'KICK', ?MCHANOP, ?ANYMODE );
?allow_chan( 'KICK', ?MHALFED, ?ANYMODE );
?allow_chan( 'TOPIC', ?MCHANOP, ?MTOPIC );
?allow_chan( 'TOPIC', ?ANYUSER, ?NOT_MTOPIC );
?allow_chan( 'PRIVMSG', ?ANYMODE, ?NOT_MMODERATED );
?allow_chan( 'PRIVMSG', ?MVOICED, ?MMODERATED );


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%  Check if an user can do the action in
%%	a chan. Check the user rights against
%%	the chan rights and IRC command to
%%	determine the authorization.
%% @end
%%
%% @spec check_law( IrcCommand, UserRight, ChanRight ) -> bool
%% where
%%		IrcCommand = atom()
%%		UserRight = int
%%		ChanRight = int
check_chanlaw( _Command, _User, _Chan) -> false.

?allow_grant( ?MCHANOP, ?MCHANOP );
?allow_grant( ?MHALFED, ?MCHANOP );
?allow_grant( ?MVOICED, ?MCHANOP bor ?MHALFED );
%?allow_grant( ?MSECRET, );
%?allow_grant( , );
check_granting( _Mode, _From ) -> false.
