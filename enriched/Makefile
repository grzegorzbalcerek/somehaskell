
all: build install clean

build:
	ghc enriched2model.hs
	ghc enriched2latex.hs
	ghc enriched2svg.hs
	ghc enriched2html.hs

clean:
	rm -rf out
	rm -f *.hi *.o
	rm -f enriched2model enriched2latex enriched2svg

install:
	mv enriched2model enriched2latex enriched2svg enriched2html ~/bin
