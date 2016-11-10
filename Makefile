# Makefile for Cornell Hold'em

RESULT = cornellholdem
SOURCES = \
  gengine.mli gengine.ml \
  opponent.mli opponent.ml player.mli player.ml \
  GUpoker.ml GUpoker.mli main.ml 
  
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)


	