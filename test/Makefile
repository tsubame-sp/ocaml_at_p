FIND = ocamlfind
OPT = ocamlopt
FLAG = -package ocaml@p -linkpkg

all: mylist.cmx test.cmx
	$(FIND) $(OPT) $(FLAG) -o test.opt mylist.cmx test.cmx

mylist.cmx:
	$(FIND) $(OPT) $(FLAG) -c mylist.ml

test.cmx:
	$(FIND) $(OPT) $(FLAG) -c test.ml

clean:
	-rm test.opt *.cm* *.o
