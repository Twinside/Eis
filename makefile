ECC=erlc
OUTPUT=all

SOLUTIONDIR=./

OBJDIR:=$(SOLUTIONDIR)ebin/
SOURCEDIR:=$(SOLUTIONDIR)src/

OBJ=$(OBJDIR)conf_loader.beam \
		$(OBJDIR)irc_log.beam \
		$(OBJDIR)irc.beam \
		$(OBJDIR)doorman.beam \
		$(OBJDIR)server_node.beam \
		$(OBJDIR)load_balancer.beam \
		$(OBJDIR)client_listener.beam \
		$(OBJDIR)chan_manager.beam \
		$(OBJDIR)ident_checker.beam \
		$(OBJDIR)eis.beam

$(OUTPUT): $(OBJ)

$(OBJDIR)%.beam: $(SOURCEDIR)%.erl
	$(ECC) -o $(OBJDIR) $<

clean:
	rm -f $(OBJDIR)*.beam

start:


