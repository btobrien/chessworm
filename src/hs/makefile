
all : tri peel unpeel pgntag nextmove mvprompt glyph

tri : Main/Tri.hs Tree/Climb.hs
	ghc -o ~/bin/tri Main/Tri.hs

peel : Main/Peel.hs Monad/Parser.hs Pgn/Parser.hs
	ghc -o ~/bin/peel Main/Peel.hs

splay : Main/Splay.hs Tree/Splay.hs Tree/Peel.hs Utils.hs 
	ghc -o ~/bin/splay2 Main/Splay.hs

unpeel : Main/Unpeel.hs Pgn/Parser.hs 
	ghc -o ~/bin/unpeel Main/Unpeel.hs

pgntag : Main/PgnTag.hs Pgn/Parser.hs 
	ghc -o ~/bin/pgntag Main/PgnTag.hs

nextmove : Main/NextMove.hs
	ghc -o ~/bin/nextmove Main/NextMove.hs

mvprompt : Main/MovePrompt.hs
	ghc -o ~/bin/mvprompt Main/MovePrompt.hs

glyph : Main/Glyph.hs
	ghc -o ~/bin/glyph Main/Glyph.hs

clean :
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
