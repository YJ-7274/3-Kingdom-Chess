.PHONY: play

clean:
	dune clean

play:
	@dune exec _build/default/src/chess.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

zip:
	rm -f 3kChess.zip
	zip -r 3kChess.zip . -x@exclude.lst

build:
	dune build