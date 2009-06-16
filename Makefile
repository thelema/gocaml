all: gocaml

gocaml: 
	ocamlbuild main.byte -lib graphics

gocaml.opt:
	ocamlbuild main.native -lib graphics

# Clean up
clean:
	ocamlbuild -clean
