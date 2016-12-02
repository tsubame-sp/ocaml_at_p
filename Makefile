#macro define
FIND = ocamlfind
OC = ocamlc
OPT = ocamlopt
FA = -package compiler-libs -linkpkg
FB = -package typpx,compiler-libs -linkpkg

LINK = helper.cmx print_typedtree.cmx insert.cmx create.cmx mod.cmx ocaml_at_p.cmx

#build
build: ppshow.cmo ppshow.cmx ocaml_at_p.opt

ppshow.cmo : ppshow.ml
	$(FIND) $(OC) $(FA) -c ppshow.ml

ppshow.cmx : ppshow.ml
	$(FIND) $(OPT) $(FA) -c ppshow.ml

#ocamlopt
ocaml_at_p.opt : helper.cmx insert.cmx print_typedtree.cmx create.cmx mod.cmx ocaml_at_p.cmx
	$(FIND) $(OPT) $(FB) -o ocaml_at_p.opt ${LINK}

helper.cmx : helper.ml
	$(FIND) $(OPT) $(FA) -c helper.ml

insert.cmx : insert.ml
	$(FIND) $(OPT) $(FA) -c insert.ml

print_typedtree.cmx : print_typedtree.ml
	$(FIND) $(OPT) $(FA) -c print_typedtree.ml

create.cmx : create.ml
	$(FIND) $(OPT) $(FA) -c create.ml

mod.cmx : mod.ml
	$(FIND) $(OPT) $(FB) -c mod.ml

ocaml_at_p.cmx : ocaml_at_p.ml
	$(FIND) $(OPT) $(FB) -c ocaml_at_p.ml

#install,uninstall
install : build
	$(FIND) install ocaml_at_p META ocaml_at_p.opt ppshow.cm* ppshow.o

remove:
	-$(FIND) remove ocaml_at_p

#clean
clean:
	-rm *.cm* *.o ocaml_at_p.opt
