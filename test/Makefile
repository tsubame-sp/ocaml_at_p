FIND = ocamlfind
OC = ocamlc
OPT = ocamlopt
FLAG = -package ocaml_at_p -linkpkg -w -A

all: mylist.cmo test.cmo
	$(FIND) $(OC) $(FLAG) -o test mylist.cmo test.cmo

opt: mylist.cmx test.cmx
	$(FIND) $(OPT) $(FLAG) -o test.opt mylist.cmx test.cmx

src: mylist.cmo
	$(FIND) $(OC) $(FLAG) -dsource -c test.ml
	$(FIND) $(OC) $(FLAG) -o test mylist.cmo test.cmo

mylist.cmo: mylist.ml
	$(FIND) $(OC) $(FLAG) -c mylist.ml

test.cmo: test.ml
	$(FIND) $(OC) $(FLAG) -c test.ml

mylist.cmx: mylist.ml
	$(FIND) $(OPT) $(FLAG) -c mylist.ml

test.cmx: test.ml
	$(FIND) $(OPT) $(FLAG) -c test.ml

clean:
	-rm test test.opt *.cm* *.o
