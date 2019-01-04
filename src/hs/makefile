
all : tri peel unpeel pgntag nextmove

tri : Trefix.hs Tri.hs
	ghc -o ~/bin/tri Tri.hs

peel : Parser.hs PgnParser.hs Peel.hs MainPeel.hs
	ghc -o ~/bin/peel MainPeel.hs

unpeel : PgnParser.hs Unpeel.hs
	ghc -o ~/bin/unpeel Unpeel.hs

pgntag : PgnParser.hs pgntag.hs
	ghc -o ~/bin/pgntag pgntag.hs

nextmove : nextmove.hs
	ghc -o ~/bin/nextmove NextMove.hs

clean :
	rm *.hi *.o
