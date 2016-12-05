main:
	ocamlfind ocamlc -w -40-26-10 -package oUnit -package lablgtk2 -linkpkg types.ml opponent.mli opponent.ml gengine.mli player.mli  gengine.ml player.ml  test.ml main.ml -o start

test:
	ocamlfind ocamlc -w -40-26-10 -package oUnit -package lablgtk2 -linkpkg types.ml opponent.mli opponent.ml gengine.mli player.mli gengine.ml player.ml  test.ml -o test && ./test && rm test

play:
	ocamlfind ocamlc -w -40-26-10 -package oUnit -package lablgtk2 -linkpkg types.ml opponent.mli opponent.ml gengine.mli player.mli  gengine.ml player.ml  main.ml -o start && ./start

testgui:
	ocamlfind ocamlc -w -40-26-10 -g -package lablgtk2 -linkpkg guitest.ml -o guitest && ./guitest && rm guitest

