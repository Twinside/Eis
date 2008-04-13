{application, eis,
 [
    {description, "Erlang IRC server"},
    {vsn, "p01"},
    {modules,
        [
            com_join
            com_kick
            com_kill
            com_notice
            com_part
            com_privmsg
            com_names
            com_who
            com_quit
            conf_loader
            irc_log
            irc_laws
            irc
            doorman
            server_node
            load_balancer
            client_listener
            chan_manager
            ident_checker
            eis
            chars
            wexpr
        ]
    },
    {registered, []},
    {env, []},
    {mod, {eis,[]}}
  ]
}

