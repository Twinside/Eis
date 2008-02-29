ECC=erlc
OUTPUT=all
OBJ= irc_log.beam \
		irc.beam \
		load_balancer.beam \
		client_listener.beam \
		chan_manager.beam \

$(OUTPUT): $(OBJ)

%.beam: %.erl
	$(ECC) -o$@ $<

clean:
	rm -f *.beam

start:


