default:
	ocamlbuild extractionLwt.cma extractionLwt.cmxa -use-ocamlfind -package lwt,lwt.unix,num

install: default
	ocamlfind install extraction-lwt META _build/extractionLwt.cmi _build/extractionLwt.cmx _build/extractionLwt.a _build/extractionLwt.cma _build/extractionLwt.cmxa _build/extractionLwt.mllib

uninstall:
	ocamlfind remove extraction-lwt

clean:
	ocamlbuild -clean
