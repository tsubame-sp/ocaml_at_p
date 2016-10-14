#Makefile for debugprint

#macro define
FIND = ocamlfind
OC = ocamlc
OPT = ocamlopt
FA = -package compiler-libs.common -linkpkg
FB = -package typpx,compiler-libs.common -linkpkg

LINK = helper.cmo insert.cmo create.cmo mod.cmo ocaml_at_p.cmo
OLINK = helper.cmx insert.cmx create.cmx mod.cmx ocaml_at_p.cmx

#build
build: ppshow.cmo ppshow.cmx ocaml_at_p.opt

ppshow.cmo : ppshow.ml
	$(FIND) $(OC) $(FA) -c ppshow.ml

ppshow.cmx : ppshow.ml
	$(FIND) $(OPT) $(FA) -c ppshow.ml

#ocamlopt
ocaml_at_p.opt : helper.cmx insert.cmx create.cmx mod.cmx ocaml_at_p.cmx
	$(FIND) $(OPT) $(FB) -o ocaml_at_p.opt ${OLINK}

helper.cmx : helper.ml
	$(FIND) $(OPT) $(FA) -c helper.ml

insert.cmx : insert.ml
	$(FIND) $(OPT) $(FA) -c insert.ml

create.cmx : create.ml
	$(FIND) $(OPT) $(FA) -c create.ml

mod.cmx : mod.ml
	$(FIND) $(OPT) $(FB) -c mod.ml

ocaml_at_p.cmx : ocaml_at_p.ml
	$(FIND) $(OPT) $(FB) -c ocaml_at_p.ml

#install,uninstall
install : build
	-$(FIND) remove ocaml@p
	$(FIND) install ocaml@p META ocaml_at_p.opt ppshow.cm* ppshow.o

uninstall:
	-$(FIND) remove ocaml@p

#clean
clean:
	-rm *.cm* *.o ocaml_at_p.opt
