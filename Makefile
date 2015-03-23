.PHONY: default

default: test.js

test.byte: test.ml
	ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml.syntax,rpclib,rpclib.js,xen-api-client.lwt,lwt,lwt.syntax,cow,cow.syntax -annot -syntax camlp4o -linkpkg -g -o test.byte test.ml

test.js: test.byte
	js_of_ocaml --noinline --pretty test.byte
