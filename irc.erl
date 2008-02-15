-module(irc).

-record(msg,
		{
			sender,		%% as a tuple : { nick, server, host } | server
			dest,		%% as string
			ircCommand,	%% as atom or int
			params,		%% as list of string
			data		%% as string
		}).

-export([
			msg_of_string/1,    %% Convert a string to an irc message
			string_of_msg/1,    %% Convert an irc message to a string
			string_of_numeric/2 %% conert a numerical irc message to string
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -> msg
msg_of_string( [$: | RawMsg] ) -> %% match if first character is ':'
	{IrcCommand, Data} = extract_irc_data( RawMsg ),
	[Sender | Next] = string:tokens( IrcCommand, " " ),
	next_parse( sender_parser( Sender ), Data, Next );
	
msg_of_string( RawMsg ) ->
	{IrcCommand, Data} = extract_irc_data( RawMsg ),
	next_parse( "", Data, string:tokens(IrcCommand, " ")).


	
next_parse( Sender, Data, [Command, Destination | Message] ) ->
	Code = extract_command_code( Command ),
	#msg{ sender=Sender,
			dest=Destination,
			ircCommand=Code,
			params=Message,
			data=Data
		};
next_parse( Sender, Data, [Command] ) ->
	Code = extract_command_code( Command ),
	#msg{ sender= Sender,
			dest = [],
			ircCommand = Code,
			params = [],
			data = Data
		}.
				
%% Separate IRC protocol information of
%% data information.
extract_irc_data( Str ) ->
	extract_irc_data( [], Str ).

extract_irc_data( Remain, [$: | Data] ) ->
	{lists:reverse(Remain), Data};
extract_irc_data( Remain, [Car | Tail] ) ->
	extract_irc_data( [Car | Remain], Tail ).
	

%% Convert an IRC command to an atom
%% or to it's numeric code.
extract_command_code( Command ) ->
	case string:to_integer( Command ) of
		{error, _} -> list_to_existing_atom( Command );
		{Numeric, _} -> Numeric
	end.

%% Parse the sender, host and user
%%
sender_parser( Msg ) ->
	case string:tokens( Msg, "!" ) of
		[Nick, Next | _ ] ->
				[User, Host | _ ] = string:tokens( Next, "@" ),
				{ Nick, User, Host };
		[Only] -> Only
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -> string
string_of_numeric( _Numeric, _Message ) ->
	error.

string_of_msg( _Message ) ->
	error.

