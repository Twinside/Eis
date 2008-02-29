ECC=erlc
OUTPUT=all
OBJ= chan_manager.beam \
		client_listener.beam \
		irc.beam \
		load_balancer.beam \

$(OUTPUT): $(OBJ)

%.beam: %.erl
	$(ECC) -o$@ $<

clean:
	rm -f *.beam

