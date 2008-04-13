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
            ,is_channame_valid/1
            ,is_username_valid/1
            ,forge_msg/4
            ,cli_to_string/1
		]).

-vsn( p01 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc
%%	Change the sender field of a msg
%% @end
%% @spec update_sender( Msg, Sender ) -> Msg
%% where
%%      Sender = string() | client()
%%      Msg = msg()
update_sender( Msg, Cli ) when is_record( Cli, client ) ->
    Msg#msg { sender = {Cli#client.nick,
                        Cli#client.username,
                        Cli#client.host} };
update_sender( Msg, Sender ) ->
	Msg#msg{ sender = Sender }.

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
    CleanedMsg = chars:cut_endline( RawMsg ),
	{IrcCommand, Data} = extract_irc_data( CleanedMsg ),
	[Sender | Next] = string:tokens( IrcCommand, " " ),
	next_parse( sender_parser( Sender ), Data, Next );
	
msg_of_string( RawMsg ) ->
    CleanedMsg = chars:cut_endline( RawMsg ),
	{IrcCommand, Data} = extract_irc_data( CleanedMsg ),
	next_parse( "", Data, string:tokens(IrcCommand, " ")).


	
next_parse( Sender, Data, [Command | Params] ) ->
	Code = extract_command_code( Command ),
	#msg{ sender=Sender,
			ircCommand=Code,
			params=Params,
			data=Data
		}.

%% @doc
%%  Transform a client or a tuple to it's IRC full
%%  host counterpart.
%% @end
%% @spec cli_to_string( Cli ) -> Result
%% where
%%      Cli = {Nick, Username, Host} | client()
%%      Result = string()
cli_to_string( #client{nick = N, username= U, host = H} ) ->
    cli_to_string( {N,U,H} );
cli_to_string( {N,U,H} ) ->
    lists:concat([N, "!", U, "@", H ]).

%% @doc
%%  Little helper function to quickly create
%%  an irc message to send. Automaticaly convert
%%  client to the good format.
%% @end
%% @spec forge_msg( Sender, Com, Params, Data ) -> Result
%% where
%%      Sender = client() | string()
%%      Com = atom()
%%      Params = [string()]
%%      Data = string()
forge_msg( Sender, Com, Params, Data ) ->     
    Msg = #msg { sender = Sender, params = Params,
                ircCommand = Com, data = Data },
    string_of_msg( Msg ).


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

irc_command_list() ->	
[
     {"PRIVMSG", 'PRIVMSG' }
    ,{ "PONG"  , 'PONG'    }
    ,{"NOTICE" , 'NOTICE'  }
    ,{ "NICK"  , 'NICK'    }
    ,{ "JOIN"  , 'JOIN'    }
    ,{  "WHO"  , 'WHO'     }
    ,{ "PART"  , 'PART'    }
    ,{ "MODE"  , 'MODE'    }
    ,{ "TOPIC" , 'TOPIC'   }
    ,{ "NAMES" , 'NAMES'   }
    ,{"INVITE" , 'INVITE'  }
    ,{ "QUIT"  , 'QUIT'    }
    ,{ "KICK"  , 'KICK'    }
    ,{ "PASS"  , 'PASS'    }
    ,{ "USER"  , 'USER'    }
    ,{ "OPER"  , 'OPER'    }
    ,{ "SQUIT" , 'SQUIT'   }
    ,{ "MOTD"  , 'MOTD'    }
    ,{"LUSERS" , 'LUSERS'  }
    ,{"VERSION", 'VERSION' }
    ,{ "STATS" , 'STATS'   }
    ,{ "LINKS" , 'LINKS'   }
    ,{  "TIME" , 'TIME'    }
    ,{"CONNECT", 'CONNECT' }
    ,{ "TRACE" , 'TRACE'   }
    ,{ "ADMIN" , 'ADMIN'   }
    ,{"SERVLIST",'SERVLIST'}
    ,{"SQUERY" , 'SQUERY'  }
    ,{ "WHOIS" , 'WHOIS'   }
    ,{"WHOWAS" , 'WHOWAS'  }
    ,{ "PING"  , 'PING'    }
    ,{ "KILL"  , 'KILL'    }
    ,{"ERRROR" , 'ERROR'   }
].

ircatom_of_text( Txt ) ->
    case lists:keysearch( Txt, 1, irc_command_list() ) of
        {value, {_,Atom}} -> Atom;
        false             -> {unknown_command, Txt}
    end.

%% Convert an IRC command to an atom
%% or to it's numeric code.
extract_command_code( Command ) ->
	case string:to_integer( Command ) of
		{error, _} -> ircatom_of_text( string:to_upper(Command) );
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

string_colapser( "", IrcCommand, Params, "" ) ->
	lists:concat( [IrcCommand, " ", chars:flat_append(Params, $   ), "\r\n" ] );

string_colapser( "", IrcCommand, Params, Data ) ->
	lists:concat( [IrcCommand, " ", chars:flat_append(Params, $   ), " :", Data, "\r\n" ] );
    
string_colapser( Sender, IrcCommand, Params, "" ) ->
	Prelude = lists:concat([":", Sender, " "]),
	lists:concat( [Prelude, IrcCommand, " ", chars:flat_append(Params, $   ), "\r\n"] );
    
string_colapser( Sender, IrcCommand, Params, Data ) ->
	Prelude = lists:concat([":", Sender, " "]),
	lists:concat( [Prelude, IrcCommand, " ",
                    chars:flat_append(Params, $   ), " :", Data, "\r\n" ] ).
                    
string_assembler( Sender, IrcCommand, Params, Data ) when is_list( IrcCommand ) ->
    string_colapser( Sender, IrcCommand, Params, Data );
string_assembler( Sender, IrcCommand, Params, Data ) when is_atom( IrcCommand ) ->
    string_colapser( Sender, atom_to_list( IrcCommand ), Params, Data );
string_assembler( Sender, IrcCommand, Params, Data ) ->
    string_colapser( Sender, integer_to_list( IrcCommand ), Params, Data ).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%%	transform a msg structure representing an IRC
%%	message to it's string representation ready to be
%%	send across the network.
%% @end
%% @spec string_of_msg( Msg ) -> Result
%% where Msg = msg()
%%		Result = string()
string_of_msg( #msg { sender=Server, ircCommand=IrcCommand,
                      params=Params, data=Data }) when is_list(Server) ->
    
        string_assembler( Server, IrcCommand, Params, Data );

string_of_msg( #msg {sender=Sender, ircCommand=IrcCommand,
				    params=Params, data=Data } ) ->
		string_assembler(cli_to_string( Sender ), IrcCommand, Params, Data).

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

%% @doc
%%  Tell if the chan name is a valid one.
%% @end
%% @spec is_channame_valid( Name ) -> bool
%% where
%%      Name = string()
is_channame_valid( [$#|Name] ) -> is_name_valid( Name, ?MAX_CHANNAME_SIZE - 1 );
is_channame_valid( [$!|Name] ) -> is_name_valid( Name, ?MAX_CHANNAME_SIZE - 1 );
is_channame_valid( [$&|Name] ) -> is_name_valid( Name, ?MAX_CHANNAME_SIZE - 1 );
is_channame_valid( [$+|Name] ) -> is_name_valid( Name, ?MAX_CHANNAME_SIZE - 1 );
is_channame_valid( _ ) -> false.

is_name_valid( _, 0 ) -> false;
is_name_valid( [], ?MAX_CHANNAME_SIZE - 1 ) -> false;
is_name_valid( [], _ ) -> true;
is_name_valid( [Char | Next], Size ) ->
    Valid = chars:is_between( Char, $-, $9 ) orelse
            chars:is_between( Char, $;, 16#FF ),
            
    if Valid -> is_name_valid( Next, Size - 1 );
       true -> false
    end.

%% @doc
%%  Tell if a nickname is valid.
%% @end
%% @spec is_username_valid( Name ) -> bool
%% where
%%      Name = string()
is_username_valid( "" ) -> false;
is_username_valid( [First|Name] ) ->
    Valid = chars:is_letter( First ) orelse
             chars:is_ircspecial( First ),
    if Valid -> is_nick_valid( Name, ?MAX_NICKNAME_SIZE - 1 );
        true -> false
    end.    

is_nick_valid( _, 0 ) -> false;
is_nick_valid( [], _ ) -> true;
is_nick_valid( [Char | Next], N ) ->
    Valid = chars:is_letter( Char ) orelse
            chars:is_ircspecial( Char ) orelse
            chars:is_digit( Char ),
    if Valid -> is_nick_valid( Next, N - 1 );
        true -> false
    end.

