TOPFILE = JGS
SOURCES = JGS.noc.ml

REWRITER_EXES = rewriter.native rewriter_ppx.native
PXFLAGS = -pp './rewriter_ppx.native -new-typenames -pp ./rewriter.native'

.SUFFIXES: .byte .native .ml .mli .cmi .cmx .cmo .noc.ml .d
.PHONY: clean all

# Put the name of the executable here
TOPFILE ?=

# Put the source files here
SOURCES ?=

OCAMLC = ocamlfind c
OCAMLOPT = ocamlfind opt
OCAMLDEP = ocamlfind dep
MLSOURCES = $(filter-out %.noc.ml, $(SOURCES))
MKSOURCES = $(filter %.noc.ml, $(SOURCES))
# -dsource --- dump a text *after* camlp5 extension
PXFLAGS ?= -syntax camlp5o -package GT-p5,OCanren.syntax,GT.syntax.all
REWRITER_EXES ?=
# byte flags
BFLAGS += -rectypes -g -package GT,OCanren
# opt flags
OFLAGS += #-inline 10
NOCANREN = noCanren
NOCFLAGS +=

.DEFAULT: all

ifeq (cleanall, $(filter clean, $(MAKECMDGOALS))$(filter all, $(MAKECMDGOALS)))
$(error Using 'clean' and 'all' targets in same invocation of $(MAKE) is discouraged)
endif

all: $(MKSOURCES:.noc.ml=.ml) $(TOPFILE).native $(TOPFILE).byte
rewriter.native:
	mkcamlp5.opt -package camlp5.pa_o,camlp5.pr_dump,GT.syntax,OCanren.syntax,GT.syntax.all -o $@ #-verbose

$(TOPFILE).native: $(MKSOURCES:.ml2mk.ml=.cmx) $(MLSOURCES:.ml=.cmx)
	$(OCAMLOPT) $(BFLAGS) $(OFLAGS) $(LIBS:.cma=.cmxa) -linkpkg $^ -o $@

$(TOPFILE).byte:  $(MKSOURCES:.noc.ml=.cmo) $(MLSOURCES:.ml=.cmo)
	$(OCAMLC)   $(BFLAGS) $(LIBS) -linkpkg $^ -o $@

clean:
	$(RM) *.cm[iox] *.annot *.o *.opt *.byte *~ *.d $(TOPFILE).native $(TOPFILE).byte $(REWRITER_EXES) \
		$(MKSOURCES:%.noc.ml=%.ml) .depend -r $(DEPDIR)

# A trick with dependecies from here: http://make.mad-scientist.net/papers/advanced-auto-dependency-generation/
DEPDIR := .deps
$(DEPDIR): ; @mkdir -p $@

$(DEPDIR)/%.ml.d: %.ml $(REWRITER_EXES)
	$(OCAMLDEP) $(PXFLAGS) $< > $@

$(DEPDIR)/%.mli.d: %.mli $(REWRITER_EXES)
	$(OCAMLDEP) $(PXFLAGS) $< > $@

DEPFILES := $(MLSOURCES:%.ml=$(DEPDIR)/%.ml.d) $(MKSOURCES:%.noc.ml=$(DEPDIR)/%.ml.d)
$(DEPFILES): $(DEPDIR)
include $(wildcard $(DEPFILES))
include $(DEPFILES)

# generic rules
%.ml: %.noc.ml $(REWRITER_EXES)
	$(NOCANREN) $(NOCFLAGS) -o $@ $<

%.cmi: %.mli $(REWRITER_EXES)  $(DEPDIR)/%.mli.d | $(DEPDIR)
	$(OCAMLC)   -c $(BFLAGS) $(PXFLAGS) $<

# Note: cmi <- mli should go first
%.cmi: %.ml $(REWRITER_EXES)  $(DEPDIR)/%.ml.d | $(DEPDIR)
	$(OCAMLC)   -c $(BFLAGS) $(PXFLAGS) $<
	#ocamlobjinfo $(<:.ml=.cmi) | head -n 6

%.cmo: %.ml $(REWRITER_EXES)  $(DEPDIR)/%.ml.d | $(DEPDIR)
	$(OCAMLC)   -c $(BFLAGS) $(PXFLAGS) $<

%.o: %.ml $(REWRITER_EXES)  $(DEPDIR)/%.ml.d | $(DEPDIR)
	$(OCAMLOPT) -c $(BFLAGS) $(STATIC) $(PXFLAGS) $(OFLAGS) $<

%.cmx: %.ml $(REWRITER_EXES)  $(DEPDIR)/%.ml.d | $(DEPDIR)
	$(OCAMLOPT) -c $(BFLAGS) $(STATIC) $(PXFLAGS) $(OFLAGS) $<
	#ocamlobjinfo $(<:.ml=.cmi) | head -n 6

NOCFLAGS = -pp ./rewriter.native -w -8 -rectypes
BFLAGS += -package noCanren.Peano -package noCanren.List

rewriter.native: rewriter_ppx.native
rewriter_ppx.native:
	ocamlfind opt -predicates ppx_driver -package OCanren-ppx.ppx_distrib,GT.ppx,GT.ppx_all,ppxlib.runner -linkpkg -linkall -o $@
