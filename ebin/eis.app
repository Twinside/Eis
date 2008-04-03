{application, eis,
 [
    {description, "Erlang IRC server"},
    {vsn, "p01"},
    {modules,
        [
            chan_manager,
            client_listener,
            com_kick,
            com_kill,
            com_notice,
            com_part,
            com_privmsg,
            conf_loader,
            doorman,
            eis,
            ident_checker,
            irc,
            irc_laws,
            irc_log,
            load_balancer,
            server_node 
        ]
    },
    {registered, []},
    {env, []},
    {mod, {eis,[]}}
  ]
}

