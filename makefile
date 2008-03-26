ECC=erlc
OUTPUT=all

SOLUTIONDIR=./


OBJDIR:=$(SOLUTIONDIR)ebin/
SOURCEDIR:=$(SOLUTIONDIR)src/
HEADERDIR:=$(SOLUTIONDIR)include/

SRCEXT=.erl
OBJEXT=.beam

modules=conf_loader \
		irc_log \
		irc_laws \
		irc \
		doorman \
		server_node \
		load_balancer \
		client_listener \
		chan_manager \
		ident_checker \
		eis

SRC:=$(addprefix $(SOURCEDIR),$(addsuffix $(SRCEXT), $(modules)))
OBJ:=$(addprefix $(OBJDIR),$(addsuffix $(OBJEXT),$(modules)))

EFLAGS:=-o $(OBJDIR) -I $(HEADERDIR)

$(OUTPUT): $(OBJ)

$(OBJDIR)%.beam: $(SOURCEDIR)%.erl
	$(ECC) $(EFLAGS) $<

docs: $(SRC)
	escript doc_generator.erl $^
	
clean:
	rm -f $(OBJDIR)*.beam

start:


