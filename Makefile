all: links

links: render

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=2>/dev/null
endif

render:
	@echo -e "LN\t$@"
	ln -s `stack exec -- which render` render

clean:
	@echo -e "CLEAN\tsymlinks"
	-rm -f render

tags:
	hasktags -c -x src tests
