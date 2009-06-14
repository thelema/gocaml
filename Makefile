OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=
OCAMLFLAGS=$(INCLUDES)  -w A
OCAMLOPTFLAGS=$(INCLUDES) 

MAIN_OBJS= options.cmo board.cmo rawgmp.cmo gmp.cmo \
	   gui.cmo history.cmo handicap.cmo main.cmo 

all : .depend gocaml

gocaml: $(MAIN_OBJS) 
	$(OCAMLC) -o gocaml graphics.cma unix.cma $(OCAMLFLAGS) $(MAIN_OBJS) 

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f gocaml .depend *.cm[iox] *~ log-*.txt

# Dependencies
.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend


