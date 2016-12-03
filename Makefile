main:
	ocamlfind ocamlc -w -40-26-10 -package oUnit -package lablgtk2 -linkpkg gengine.mli player.mli opponent.mli GUpoker.mli gengine.ml player.ml opponent.ml GUpoker.ml test.ml main.ml -o start

test:
	ocamlfind ocamlc -w -40-26-10 -package oUnit -package lablgtk2 -linkpkg gengine.mli player.mli opponent.mli GUpoker.mli gengine.ml player.ml opponent.ml GUpoker.ml test.ml -o test && ./test && rm test

play:
	ocamlfind ocamlc -w -40-26-10 -package oUnit -package lablgtk2 -linkpkg gengine.mli player.mli opponent.mli GUpoker.mli gengine.ml player.ml opponent.ml GUpoker.ml main.ml -o start && ./start

testgui:
	ocamlfind ocamlc -w -40-26-10 -g -package lablgtk2 -linkpkg guitest.ml -o guitest && ./guitest && rm guitest

