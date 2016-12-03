main: 
	ocamlfind ocamlc -package oUnit -package lablgtk2 -linkpkg gengine.mli player.mli opponent.mli GUpoker.mli gengine.ml player.ml opponent.ml GUpoker.ml test.ml main.ml -o start

test:
	ocamlfind ocamlc -package oUnit -package lablgtk2 -linkpkg gengine.mli player.mli opponent.mli GUpoker.mli gengine.ml player.ml opponent.ml GUpoker.ml test.ml -o test && ./test && rm test

play:
	ocamlfind ocamlc -package oUnit -package lablgtk2 -linkpkg gengine.mli player.mli opponent.mli GUpoker.mli gengine.ml player.ml opponent.ml GUpoker.ml main.ml -o start && ./start