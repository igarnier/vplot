default:
	jbuilder build

all:
	jbuilder build
	jbuilder build @doc

install:
	jbuilder install

clean:
	jbuilder clean
