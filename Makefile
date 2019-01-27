default:
	dune build
.PHONY: default

all:
	dune build
	dune build @doc
.PHONY: all

install:
	dune install
.PHONY: install

clean:
	dune clean
.PHONY: clean

test:
	dune runtest
.PHONY: test
