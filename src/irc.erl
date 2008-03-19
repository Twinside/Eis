-module(irc).

-include("irc_struct.hrl").

-export([
			msg_of_string/1,    %% Convert a string to an irc message
			string_of_msg/1,    %% Convert an irc message to a string
			send_ident_msg/2
]).

-vsn( p01 ).

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
string_assembler( Sender, Dest, IrcCommand, Params, Data ) ->
	SendedCommand = if is_atom(IrcCommand) -> atom_to_list(IrcCommand);
						true -> integer_to_list(IrcCommand)
					end,
	Prelude = if Sender == "" -> "";
				true -> lists:concat([":", Sender, " "])
			end,
	lists:concat( [Prelude, SendedCommand, " ", Dest, " ", lists:append(Params), ":", Data] ).


string_of_msg( Msg ) ->
	case Msg of
		#msg {sender={Nick,Username,Host},
				dest=Dest,
				ircCommand=IrcCommand,
				params=Params,
				data=Data } ->
			string_assembler(lists:concat([Nick, "!", Username, "@", Host ]),
											Dest, IrcCommand, Params, Data);
		#msg { sender=Server,
				dest=Dest,
				ircCommand=IrcCommand,
				params=Params,
				data=Data } ->
			string_assembler( Server, Dest, IrcCommand, Params, Data )
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A little function to send quickly a notice during identification
send_ident_msg( CliSock, Message ) ->
	gen_tcp:send( CliSock, "NOTICE AUTH " ++ Message ).

