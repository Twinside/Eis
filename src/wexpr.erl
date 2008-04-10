-module( wexpr ).

-vsn( p01 ).

-export([
            match/2
        ]).


%% @doc
%%  Match a string against a wildcard patern.
%%  <p>
%%      Special caracters :
%%      <ul>
%%          <li> * : match 0 or n characters, can be any character</li>
%%          <li> ? : match 1 character, can be any character</li>
%%          <li> \* : match a '*' in the text </li>
%%          <li> \? : match a '?' in the text </li>
%%      </ul>
%%  </p>
%% @end
%% @spec match( Patern, Str ) -> bool
%% where
%%      Patern = string()
%%      Str = string()
match(        [], []  ) -> true;
match(         _, []  ) -> false;
match(        [], _   ) -> false;
match( [F|PNext], Str ) -> matching( F, PNext, Str ).

% terminaison reason
matching( _ , [], [] ) -> true;
matching(  C, [], [C]) -> true;
matching( $*, [], _ ) -> true;
matching( _, _, [] ) -> false;

% when we should ignore the wildcard '*'
matching( $\\, [$*|PNext], [$*|Next] ) -> match( PNext, Next );
matching( $\\, [$*|    _],       _   ) -> false;

% when we should ignore the wildcard '?'
matching( $\\, [$?|PNext], [$?|Next] ) -> match( PNext, Next );
matching( $\\, [$?|  _  ],         _ ) -> false;

% matching the '?' wildcard, it's supposed
% to be only a letter.
matching( $?, [NChar|PNext], [_|Next] ) ->
    matching( NChar, PNext, Next );

% matching the '*' wildcard.
% in this implementation, matching against a *
% can make the algorithm n² or worst.
matching( $*, [C|PNext], [C|Next] ) ->
    Matched = match( PNext, Next ),
    if Matched -> true;
          true -> matching( $*, [C|PNext], Next )
    end;    

matching( $*, Patern, [_|Next] ) ->
    matching( $*, Patern, Next );

% we're matching 'normal' letter
% against 'normal' letter.
matching( C, [Nc | PNext], [C|Next] ) ->
    matching( Nc, PNext, Next );

% we really failed the match.
matching( _C, _PaterLeft, _StrLeft ) -> false.

