%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% <p>
%%		irc module is the collection of helper function related
%%		to IRC.
%% </p>
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(irc).

-include("irc_struct.hrl").

-export([
			msg_of_string/1
			,string_of_msg/1
			,send_ident_msg/2
			,prepare_err/2
			,update_sender/2
            ,send_err/3
		]).

-vsn( p01 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc
%%	Change the sender field of a msg
%% @end
%% @spec update_sender( Msg, Sender ) -> Msg
update_sender( Msg, Sender ) ->
	#msg{ sender = Sender,
			ircCommand = Msg#msg.ircCommand,
			params = Msg#msg.params,
			data = Msg#msg.data
		}.

%% @doc
%%	Transform a text representation of IRC
%%	(received by a socket by example), and structure
%%	it in a msg struct.
%% @end
%% @spec msg_of_string( Msg ) -> Result
%% where
%%		Msg = string
%%		Result = msg
msg_of_string( [$: | RawMsg] ) -> %% match if first character is ':'
	{IrcCommand, Data} = extract_irc_data( RawMsg ),
	[Sender | Next] = string:tokens( IrcCommand, " " ),
	next_parse( sender_parser( Sender ), Data, Next );
	
msg_of_string( RawMsg ) ->
	{IrcCommand, Data} = extract_irc_data( RawMsg ),
	next_parse( "", Data, string:tokens(IrcCommand, " ")).


	
next_parse( Sender, Data, [Command | Params] ) ->
	Code = extract_command_code( Command ),
	#msg{ sender=Sender,
			ircCommand=Code,
			params=Params,
			data=Data
		}.
				
%% Separate IRC protocol information of
%% data information.
extract_irc_data( Str ) ->
	extract_irc_data( [], Str ).

extract_irc_data( Remain, [$: | Data] ) ->
	{lists:reverse(Remain), Data};
extract_irc_data( Remain, [Car | Tail] ) ->
	extract_irc_data( [Car | Remain], Tail );
extract_irc_data( InvertedMsg, [] ) ->
	{lists:reverse( InvertedMsg ), ""}.
	

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
string_assembler( Sender, IrcCommand, Params, Data ) ->
	SendedCommand = if is_atom(IrcCommand) -> atom_to_list(IrcCommand);
						true -> integer_to_list(IrcCommand)
					end,
	Prelude = if Sender == "" -> "";
				true -> lists:concat([":", Sender, " "])
			end,
	lists:concat( [Prelude, SendedCommand, " ", lists:append(Params), ":", Data] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	transform a msg structure representing an IRC
%%	message to it's string representation ready to be
%%	send across the network.
%% @end
%% @spec string_of_msg( Msg ) -> Result
%% where Msg = msg()
%%		Result = string()
string_of_msg( Msg ) ->
	case Msg of
		#msg {sender={Nick,Username,Host},
				ircCommand=IrcCommand,
				params=Params,
				data=Data } ->
			string_assembler(lists:concat([Nick, "!", Username, "@", Host ]),
											IrcCommand, Params, Data);
		#msg { sender=Server,
				ircCommand=IrcCommand,
				params=Params,
				data=Data } ->
			string_assembler( Server, IrcCommand, Params, Data )
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% A little function to send quickly a notice during identification
%% @end
send_ident_msg( CliSock, Message ) ->
	ok = gen_tcp:send( CliSock, "NOTICE AUTH " ++ Message )
	.

%% @doc
%%	Prepare a server message to send as a numerical error.
%% @end
%% @spec prepare_err( Serverhost, Errmsg ) -> Result
%% where
%%		Serverhost = string()
%%		Errmsg = string() | [string]
%%		Result = string()
prepare_err( Serverhost, Errmsg ) ->
	":" ++ Serverhost ++ Errmsg
	.
%% @doc
%%  Given a thread, a client and an error message
%%  send him an error. It's for numerical error reply.
%% @end
%% @spec send_err( State, Client, Errmsg ) -> none
%% where
%%      State = string() | listener() | cmanager()
%%      Client = client()
%%      Errmsg = string()
send_err( State, Client, Errmsg )
        when is_record( State, listener ) ->
        send_err( State#listener.server_host, Client, Errmsg );
send_err( State, Client, Errmsg )
        when is_record( State, cmanager ) ->
        send_err( State#cmanager.server_host, Client, Errmsg );

send_err( ServHost, Client, Errmsg ) ->
    ParamErr = prepare_err( ServHost, Errmsg ),
    (Client#client.send)( Client#client.sendArgs, ParamErr )
    .
