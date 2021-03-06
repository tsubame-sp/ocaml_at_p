#macro define
FIND = ocamlfind
OC = ocamlc
OPT = ocamlopt
FA = -package compiler-libs -linkpkg
FB = -package typpx,compiler-libs -linkpkg

LINK = helper.cmx insert.cmx create.cmx mod.cmx ocaml_at_p.cmx

#build
build: print.cmo print.cmx ocaml_at_p.opt

print.cmo : print.ml
	$(FIND) $(OC) $(FA) -c print.ml

print.cmx : print.ml
	$(FIND) $(OPT) $(FA) -c print.ml

#ocamlopt
ocaml_at_p.opt : helper.cmx insert.cmx create.cmx mod.cmx ocaml_at_p.cmx
	$(FIND) $(OPT) $(FB) -o ocaml_at_p.opt ${LINK}

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
	$(FIND) install ocaml_at_p META ocaml_at_p.opt print.cm* print.o

remove:
	-$(FIND) remove ocaml_at_p

#clean
clean:
	-rm *.cm* *.o ocaml_at_p.opt

reset: clean remove

reinstall: reset install
