%% @doc
%%  The idea of this module is to keep track
%%  of function to send a specific error message.
%%  Useful in many case, some error messages are
%%  duplicated over many commands.
%% @end
-module( err ).

-include( "irc_struct.hrl" ).

-vsn( p01 ).

-export([
            usernotinchannel/4
            ,chanopprivsneeded/3
            ,notonchannel/3
        ]).

%% @doc
%%  Send an ERR_USERNOTINCHANNEL to Cli with
%%  the specified Target nickname and a Chan
%% @end
%% @spec usernotinchannel( State, Cli, TargetNick, Chan ) -> true
%% where
%%      State = listener() | cmanager()
%%      Cli = client()
%%      Chan = chan() | string
%%      TargetNick = string()
usernotinchannel( State, Cli, TargetNick, Chan ) when is_record( Chan, chan )->
    usernotinchannel( State, Cli, TargetNick, Chan#chan.channame );

usernotinchannel( State, Cli, TargetNick, Chan ) ->
    Errmsg = ?ERR_USERNOTINCHANNEL
            ++ TargetNick
            ++ [$  | Chan#chan.channame]
            ++ [$  | ?ERR_USERNOTINCHANNEL_TXT],
    irc:send_err( State, Cli, Errmsg ).

%% @doc
%%  Send a chanopprivsneeded error message to Cli.
%% @end 
%% @spec chanopprivsneeded( State, Cli, TargetNick, Chan ) -> true
%% where
%%      State = listener() | cmanager()
%%      Cli = client()
%%      Chan = chan() | string()
chanopprivsneeded( State, Cli, Chan ) when is_record( Chan, chan ) ->
    chanopprivsneeded( State, Cli, Chan#chan.channame );
    
chanopprivsneeded( State, Cli, Chan ) ->
    Errmsg = ?ERR_CHANOPPRIVSNEEDED
         ++ [$  | Chan ]
         ++ [$  | ?ERR_CHANOPPRIVSNEEDED_TXT],
    irc:send_err( State, Cli, Errmsg ).

%% @spec notonchannel( State, Cli, TargetNick, Chan ) -> true
%% where
%%      State = listener() | cmanager()
%%      Cli = client()
%%      Chan = chan() | string()
notonchannel( State, Cli, Chan ) when is_record( Chan, chan ) ->
    notonchannel( State, Cli, Chan#chan.channame );

notonchannel( State, Cli, Chan ) ->
    Errmsg = ?ERR_NOTONCHANNEL
           ++ Cli#client.nick
           ++ [$  | Chan ]
           ++ [$  | ?ERR_NOTONCHANNEL_TXT],
           irc:send_err( State, Cli, Errmsg ).

