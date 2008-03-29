ECC=erlc
OUTPUT=all

SOLUTIONDIR=./

OBJDIR:=$(SOLUTIONDIR)ebin/
SOURCEDIR:=$(SOLUTIONDIR)src/
COMSOURCEDIR:=$(SOURCEDIR)commands/
HEADERDIR:=$(SOLUTIONDIR)include/


SRCEXT=.erl
OBJEXT=.beam

irccommands=com_join \
			com_kick \
			com_kill \
			com_notice \
			com_part \
			com_privmsg

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

COMOBJ:=$(addprefix $(OBJDIR),$(addsuffix $(OBJEXT),$(irccommands)))
COMSRC:=$(addprefix $(COMSOURCEDIR),$(addsuffix $(SRCEXT), $(irccommands)))
SRC:=$(addprefix $(SOURCEDIR),$(addsuffix $(SRCEXT), $(modules)))
OBJ:=$(addprefix $(OBJDIR),$(addsuffix $(OBJEXT),$(modules)))

ALLSOURCES:=$(SRC) $(COMSRC)
ALLOBJ:=$(OBJ) $(COMOBJ)

EFLAGS:=-o $(OBJDIR) -I $(HEADERDIR)

$(OUTPUT): $(ALLOBJ)

all: $(OUTPUT) docs

$(OBJDIR)%.beam: $(SOURCEDIR)%.erl
	$(ECC) $(EFLAGS) $<

$(OBJDIR)%.beam: $(COMSOURCEDIR)%.erl
	$(ECC) $(EFLAGS) $<
	
docs: $(ALLSOURCES)
	escript doc_generator.erl $^
	
clean:
	rm -f $(OBJDIR)*.beam doc/*

start:


