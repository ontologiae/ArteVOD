all: 
	ocamlfind ocamlopt -verbose  -package "netclient,unix,xml-light,extlib" arte.ml -linkpkg  -o arte

clean:
	rm *.cm*
	rm arte
