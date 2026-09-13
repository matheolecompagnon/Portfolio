# Makefile du projet de INPF12

FICHIER=projet.ml 
BIN=test

$(BIN): projet.cmo
	ocamlc -g projet.cmo -o $@

%.cmo : %.ml 
	ocamlc -g -c $<

%.cmi : %.mli
	ocamlc -g -c $<


clean : 
	rm -f projet.cmo projet.cmi $(BIN)
