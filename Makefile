all: gocaml

gocaml: 
	ocamlbuild main.byte

gocaml.opt:
	ocamlbuild main.native

# Clean up
clean:
	ocamlbuild -clean
