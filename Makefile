#Makefile for debugprint

#macro define
FIND = ocamlfind
OC = ocamlc
FA = -w -A -package compiler-libs.common -linkpkg
FB = -w -A -package typpx,compiler-libs.common -linkpkg

LINK = helper.cmo insert.cmo create.cmo mod.cmo ocaml@p.cmo

#ocamlc
build: ocaml@p

ocaml@p : ppshow.cmo helper.cmo insert.cmo create.cmo mod.cmo ocaml@p.cmo
	$(FIND) $(OC) $(FB) -o ocaml@p ${LINK}

ppshow.cmo : ppshow.ml
	$(FIND) $(OC) $(FA) -c ppshow.ml

helper.cmo : helper.ml
	$(FIND) $(OC) $(FA) -c helper.ml

insert.cmo : insert.ml
	$(FIND) $(OC) $(FA) -c insert.ml

create.cmo : create.ml
	$(FIND) $(OC) $(FA) -c create.ml

mod.cmo : create.ml
	$(FIND) $(OC) $(FB) -c mod.ml

ocaml@p.cmo : ocaml@p.ml
	$(FIND) $(OC) $(FB) -c ocaml@p.ml

install : build
	$(FIND) install ocaml@p META ocaml@p ppshow.cm*

uninstall:
	-$(FIND) remove ocaml@p

clean:
	-rm *.cm* ocaml@p
