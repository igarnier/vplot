default:
	jbuilder build
.PHONY: default

all:
	jbuilder build
	jbuilder build @doc
.PHONY: all

install:
	jbuilder install
.PHONY: install

clean:
	jbuilder clean
.PHONY: clean

test:
	jbuilder runtest
.PHONY: test
