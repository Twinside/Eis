ECC=erlc
OUTPUT=all
OBJ= irc_log.beam \
		irc.beam \
		doorman.beam \
		server_node.beam \
		load_balancer.beam \
		client_listener.beam \
		chan_manager.beam \
		ident_checker.beam

$(OUTPUT): $(OBJ)

%.beam: %.erl
	$(ECC) -o$@ $<

clean:
	rm -f *.beam

start:


