%
% Containt the basic structures used by the server to
% store content.
%

-define( RPL_UMODEIS, " 221 " ).
-define( RPL_WHOREPLY, " 352 " ).

-define( RPL_ENDOFWHO, " 315 " ).
-define( RPL_ENDOFWHO_TXT, ":End of /WHO list\r\n" ).

-define( RPL_NAMEREPLY, " 353 " ).

-define( RPL_ENDOFNAMES, " 366 " ).
-define( RPL_ENDOFNAMES_TXT, ":End of /Names list\r\n" ).

-define( RPL_BANLIST, " 367 " ).

-define( RPL_ENDOFBANLIST, " 368 " ).
-define( RPL_ENDOFBANLIST_TXT, " :End of ban list\r\n" ).

-define( RPL_CHANNELMODEIS, " 324 " ).

% Defining IRC error codes for use when
% replying with a numerical code :)
-define( ERR_NEEDMOREPARAMS, "461 :Error need more parameters\r\n" ).
-define( ERR_ALREADYREGISTERED, 462 ).

-define( ERR_BADCHANNELKEY, "475" ).
-define( ERR_BADCHANNELKEY_TXT, ":Error bad channel key" ).

-define( ERR_CHANNELISFULL, "471" ).
-define( ERR_CHANNELISFULL_TXT, ": Error channel is full\r\n" ).

-define( ERR_NOSUCHNICK, " 401 " ).
-define( ERR_NOSUCHNICK_TXT, " :Error no such nick " ).

-define( ERR_NOSUCHCHANNEL, " 403 " ).
-define( ERR_NOSUCHCHANNEL_TXT, " :Error no such channel " ).

-define( ERR_CANNOTSENDTOCHAN, " 404 " ).
-define( ERR_CANNOTSENDTOCHAN_TXT, " :Cannot send to channel" ).

-define( ERR_TOOMANYCHANNELS, " 405 " ).
-define( ERR_TOOMANYCHANNELS_TXT, " :You have joined too many channels\r\n" ).

-define( ERR_UNKNOWNCOMMAND, "421" ).
-define( ERR_UNKNWONCOMMAND_TXT, "Unknown command " ).

-define( ERR_BANNEDFROMCHAN, "461" ).
-define( ERR_BANNEDFROMCHAN_TXT, ": You are banned from the chan" ).

-define( ERR_NORECIPIENT, "411" ).
-define( ERR_NORECIPIENT_TXT, " : There is no recipient in your command\r\n" ).

-define( ERR_NOTEXTTOSEND, "412" ).
-define( ERR_NOTEXTTOSEND_TXT, ": There is no text to send\r\n" ).

-define( ERR_NOTONCHANNEL, " 442 " ).
-define( ERR_NOTONCHANNEL_TXT, ": you're not on that channel\r\n" ).

-define( ERR_NOPRIVILEGES, " 481 " ).
-define( ERR_NOPRIVILEGES_TXT, ":Permission denied, you're not an IRC operator\r\n" ).

-define( ERR_UNKNOWNMODEFLAG, " 501 " ).
-define( ERR_UNKNOWNMODEFLAG_TXT, ":Unknown mode flag " ).

-define( ERR_USERDONTMATCH, " 502 " ).
-define( ERR_USERDONTMATCH_TXT, ":Cannot change mode for other users\r\n" ).

-define( RPL_TOPIC, "322 " ).
-define( RPL_NOTOPIC, "331 " ).

-define( RPL_WELCOME, 001 ).
-define( RPL_YOURHOST, 002 ).
-define( RPL_CREATED, 003 ).
-define( RPL_MYINFO, 004 ).
-define( RPL_BOUNCE, 005 ).

-define( RPL_MOTDSTART, 375 ).
-define( RPL_MOTD, 372 ).
-define( RPL_ENDOFMOTD, 476 ).
