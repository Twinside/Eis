{application, eis,
 [
    {description, "Erlang IRC server"},
    {vsn, "p01"},
    {modules,
        [
            chan_manager.beam,
            client_listener.beam,
            com_kick.beam,
            com_kill.beam,
            com_notice.beam,
            com_part.beam,
            com_privmsg.beam,
            conf_loader.beam,
            doorman.beam,
            eis.beam,
            ident_checker.beam,
            irc.beam,
            irc_laws.beam,
            irc_log.beam,
            load_balancer.beam,
            server_node.beam 
        ]
    },
    {registered, []},
    {env, []},
    {mod, {,[]}}
  ]
}
