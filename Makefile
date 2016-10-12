#Makefile for debugprint

#macro define
FIND = ocamlfind
OC = ocamlc
FLAG = -w -A -package typpx,compiler-libs.common -linkpkg

COMPo = ppshow.ml helper.ml exp_to_s.ml insert.ml create.ml mod.ml ocaml@p.ml
LINKo = helper.cmo exp_to_s.cmo insert.cmo create.cmo mod.cmo ocaml@p.cmo

COMP = ppshow.ml helper.ml insert.ml create.ml mod.ml ocaml@p.ml
LINK = helper.cmo insert.cmo create.cmo mod.cmo ocaml@p.cmo

#ocamlc
all: comp ins

comp: clean
	$(FIND) $(OC) $(FLAG) -c ${COMP}
	$(FIND) $(OC) $(FLAG) -o ocaml@p ${LINK}

ins:
	-$(FIND) remove ocaml@p
	$(FIND) install ocaml@p META ocaml@p ppshow.cm*

install:
	cp ocaml@p /Users/kenji/.opam/4.03.0/lib/ocaml@p/
	cp ppshow.cm* /Users/kenji/.opam/4.03.0/lib/ocaml@p/

clean:
	-rm *.cm* ocaml@p
