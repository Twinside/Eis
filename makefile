##########################################
# Global configuration
#
##########################################
OUTPUT=all

SRCEXT=.erl
OBJEXT=.beam
SOLUTIONDIR=./

OBJDIR      := $(SOLUTIONDIR)ebin/
SOURCEDIR   := $(SOLUTIONDIR)src/
COMSOURCEDIR:= $(SOURCEDIR)commands/
HEADERDIR   := $(SOLUTIONDIR)include/

###########################################
# Compiler configuration, put here all
# you want to pass to $(ECC)
##########################################
ECC=erlc
DEBUG=+debug_info
EFLAGS:=$(DEBUG) -o $(OBJDIR) -I $(HEADERDIR) -Wall


###########################################
# Test configuration
# 
###########################################
TESTDIR     :=$(SOLUTIONDIR)test/
TESTCOMDIR  :=$(TESTDIR)commands/
TESTTAG     :=_test
TESTSUFIX   :=$(TESTTAG)$(SRCEXT)
TESTOBJSUFIX:=$(TESTTAG)$(OBJEXT)

###########################################
# List of compiled modules, add one here
# if you need.
###########################################
irccommands:=com_join \
			com_kick \
			com_kill \
			com_notice \
			com_part \
			com_privmsg \
            com_names \
            com_who \
            com_quit \
            com_mode \
	    com_topic

modules:=conf_loader \
		irc_log \
		irc_laws \
		irc \
		doorman \
		server_node \
		load_balancer \
		client_listener \
		chan_manager \
		ident_checker \
		eis \
        chars \
        wexpr

recompile_header:=  irc_struct.hrl \
                    irc_text.hrl

############################################################
# Here we generate all the required file names.
# do not touch unless you really know what you're doing.
############################################################

COMOBJ:=$(addprefix $(OBJDIR),$(addsuffix $(OBJEXT),$(irccommands)))
COMSRC:=$(addprefix $(COMSOURCEDIR),$(addsuffix $(SRCEXT), $(irccommands)))
SRC:=$(addprefix $(SOURCEDIR),$(addsuffix $(SRCEXT), $(modules)))
OBJ:=$(addprefix $(OBJDIR),$(addsuffix $(OBJEXT),$(modules)))

TESTMODULES:=$(addsuffix $(TESTTAG), $(irccommands)) \
             $(addsuffix $(TESTTAG), $(modules))

HDEP:=$(addprefix $(HEADERDIR), $(recompile_header))

TSTSRC:=$(addprefix $(TESTDIR), $(addsuffix $(TESTSUFIX), $(modules))) \
        $(addprefix $(TESTCOMDIR), $(addsuffix $(TESTSUFIX), $(irccommands)))
TSTOBJ:=$(addprefix $(OBJDIR),$(addsuffix $(TESTOBJSUFIX), $(modules))) \
        $(addprefix $(OBJDIR), $(addsuffix $(TESTOBJSUFIX), $(irccommands)))
 
ALLSOURCES:=$(SRC) $(COMSRC)
ALLOBJ:=$(OBJ) $(COMOBJ)
############################################################
# Here it's the "standard" make rules.
#
############################################################
$(OUTPUT): $(ALLOBJ)

####
# Software build
$(OBJDIR)%$(OBJEXT): $(SOURCEDIR)%$(SRCEXT) $(HDEP)
	$(ECC) $(EFLAGS) $<

$(OBJDIR)%$(OBJEXT): $(COMSOURCEDIR)%$(SRCEXT) $(HDEP)
	$(ECC) $(EFLAGS) $<

#####
# for tests
$(OBJDIR)%$(OBJEXT): $(TESTDIR)%$(SRCEXT) $(HDEP)
	$(ECC) $(EFLAGS) $<

$(OBJDIR)%$(OBJEXT): $(TESTCOMDIR)%$(SRCEXT) $(HDEP)
	$(ECC) $(EFLAGS) $<

#####
# doc generation
docs: $(ALLSOURCES)
	escript doc_generator.erl $^

test: $(TSTOBJ) $(OBJDIR)tests.beam

#####
# test running.
tests: $(ALLBOJ) test
	escript test_generator.erl $(TESTMODULES)

clean:
	rm -f $(OBJDIR)*.beam doc/*

start:


