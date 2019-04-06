# Makefile for creating mlyacc.sml and base.sml files.
# Created: David Tarditi (tarditi@cs.cmu.edu) 12/4/89
#
# Description:
#   This makefile can creates two different files:
#	smlyacc.sml: SML file for ML-Yacc.  Load it to create the ParseGen
#		     structure.
#	base.sml: SML file which must be loaded before any files produced
#		  smlyacc are loaded.  This file contains background
#		  signatures, the polymorphic parser structure, and the
#		  functors to join lexers and parsers.
#
#		  It comes in two variants: one with a vanilla parser
#		  and another with an error-correcting parser
#
#    The makefile does not have an entry to create base.sml -- the user
#    must explicitly do this.  This forces the user to consciously choose
#    the parser to be created.

# create smlyacc file from parts

SRC = src
OBJ = base.sml $(SRC)/hdr.sml	\
     $(SRC)/yacc.grm.sig $(SRC)/yacc.grm.sml $(SRC)/yacc.lex.sml \
     $(SRC)/parse.sml $(SRC)/utils.sig $(SRC)/utils.sml	\
     $(SRC)/sigs.sml $(SRC)/grammar.sml $(SRC)/core.sml \
     $(SRC)/grammar.sml $(SRC)/core.sml $(SRC)/coreutils.sml \
     $(SRC)/graph.sml $(SRC)/graph.sml $(SRC)/look.sml	\
     $(SRC)/lalr.sml $(SRC)/mkstruct.sml $(SRC)/mktable.sml $(SRC)/verbose.sml \
     $(SRC)/yacc.sml
BASESRC = $(SRC)/base.sig $(SRC)/stream.sml $(SRC)/lrtable.sml $(SRC)/join.sml

smlyacc.sml: $(OBJ);
	 cat $(OBJ) > smlyacc.sml

# create error correcting parser

ec:	$(BASESRC) $(SRC)/parser2.sml;
	cat $(BASESRC) $(SRC)/parser2.sml > base.sml

# create vanilla parser

vanilla: $(BASESRC) $(SRC)/parser1.sml;
	cat $(BASESRC) $(SRC)/parser1.sml > base.sml
