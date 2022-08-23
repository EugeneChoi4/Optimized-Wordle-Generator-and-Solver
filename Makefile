.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

entr:
	OCAML3RUNPARAM=b dune exec entropy_bin/entropy_main.exe

script:
	OCAMLRUNPARAM=b dune exec scripts/to_json.exe

zip:
	rm -f 3110dle.zip
	zip -r 3110dle.zip . -x@exclude.lst

clean:
	dune clean
	rm -f 3110dle.zip

docs:
	dune build @doc



count:
	dune clean
	rm -f 3110dle.zip
	cloc --by-file --include-lang=OCaml .
	dune build