main: 
	ocamlbuild -pkgs oUnit main.byte

test:
	ocamlbuild -pkgs oUnit test.byte && ./test.byte
