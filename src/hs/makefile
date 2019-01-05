
all : tri peel unpeel pgntag nextmove

tri : Main/Tri.hs Tree/Climb.hs
	ghc -o ~/bin/tri Main/Tri.hs

peel : Main/Peel.hs Monad/Parser.hs Pgn/Parser.hs Tree/Rose.hs
	ghc -o ~/bin/peel Main/Peel.hs

unpeel : Main/Unpeel.hs Pgn/Parser.hs 
	ghc -o ~/bin/unpeel Main/Unpeel.hs

pgntag : Main/PgnTag.hs Pgn/Parser.hs 
	ghc -o ~/bin/pgntag Main/PgnTag.hs

nextmove : Main/NextMove.hs
	ghc -o ~/bin/nextmove Main/NextMove.hs

clean :
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
