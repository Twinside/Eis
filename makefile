##########################################
# Global configuration
#
##########################################
OUTPUT      :=all

SRCEXT      :=.erl
OBJEXT      :=.beam
SOLUTIONDIR :=./

OBJDIR      := $(SOLUTIONDIR)ebin/
SOURCEDIR   := $(SOLUTIONDIR)src/
COMSOURCEDIR:= $(SOURCEDIR)commands/
HEADERDIR   := $(SOLUTIONDIR)include/
APP         := $(OBJDIR)eis.app

###########################################
# Compiler configuration, put here all
# you want to pass to $(ECC)
###########################################
ECC         :=erlc
DEBUG       :=+debug_info
EFLAGS      :=$(DEBUG) -o $(OBJDIR) -I $(HEADERDIR) -Wall

###########################################
# Helper variables which can be used in
# different subscript.
###########################################
COMMA       :=, 
EMPTY       :=
SPACE       :=$(EMPTY) $(EMPTY)

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
        pinger \
        wexpr \
        err

recompile_header:=  irc_struct.hrl \
                    irc_text.hrl

############################################################
# Here we generate all the required file names.
# do not touch unless you really know what you're doing.
############################################################

COMOBJ  :=$(addprefix $(OBJDIR),$(addsuffix $(OBJEXT),$(irccommands)))
COMSRC  :=$(addprefix $(COMSOURCEDIR),$(addsuffix $(SRCEXT), $(irccommands)))
SRC     :=$(addprefix $(SOURCEDIR),$(addsuffix $(SRCEXT), $(modules)))
OBJ     :=$(addprefix $(OBJDIR),$(addsuffix $(OBJEXT),$(modules)))

TESTMODULES:=$(addsuffix $(TESTTAG), $(irccommands)) \
             $(addsuffix $(TESTTAG), $(modules))

HDEP    :=$(addprefix $(HEADERDIR), $(recompile_header))

TSTSRC  :=$(addprefix $(TESTDIR), $(addsuffix $(TESTSUFIX), $(modules))) \
          $(addprefix $(TESTCOMDIR), $(addsuffix $(TESTSUFIX), $(irccommands)))
TSTOBJ  :=$(addprefix $(OBJDIR),$(addsuffix $(TESTOBJSUFIX), $(modules))) \
          $(addprefix $(OBJDIR), $(addsuffix $(TESTOBJSUFIX), $(irccommands)))
 
ALLSOURCES:=$(SRC) $(COMSRC)
ALLOBJ:=$(OBJ) $(COMOBJ)
############################################################
# Here it's the "standard" make rules.
#
############################################################
$(OUTPUT): $(ALLOBJ) $(APP)

# creation of the app file.
# now it's automatically updated with
# each change of the makefile :)
MODLIST   := $(modules) $(irccommands)
ALLMODULE := $(subst $(SPACE),$(COMMA),$(strip $(MODLIST)))
$(APP): eisapp.header eisapp.footer makefile
	echo ** Generating app file...
	@cat eisapp.header > $@
	@echo $(ALLMODULE) >> $@
	@cat eisapp.footer >> $@
    
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

ldoc: docs
	xsltproc -o doc/conf_doc.html conf.xslt conf.xml

lconf:
	xsltproc -o ebin/eis.conf conf_real.xslt conf.xml

wdoc: docs
	msxsl conf.xml conf.xslt -o doc\conf_doc.html
    
wconf: docs
	msxsl conf.xml conf_real.xslt -o ebin/eis.conf
    
test: $(TSTOBJ) $(OBJDIR)tests.beam

    
help:
	@echo ======== Eis Makefile help ==========
	@echo make       : build the software and .app file
	@echo make test  : build the test suite.
	@echo make tests : perform the test suite
	@echo make docs  : compile the documentation from source code
	@echo make help  : show this screen
	@echo make ldoc  : create doc and conf's documentation in linux
	@echo make wdoc  : create doc and conf's documentation in windows
	@echo make lconf : generate conf file from xml under linux
	@echo make wconf : generate conf file from xml under windows

#####
# test running.
tests: $(ALLBOJ) test
	escript test_generator.erl $(TESTMODULES)
	
#####
# running application
start: all
	cd ebin
	erl -run eis dlaunch

clean:
	rm -f $(OBJDIR)*.beam doc/*

start:


