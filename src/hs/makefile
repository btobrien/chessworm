
all : zipline peel unpeel pgntag nextmove mvprompt glyph splay

zipline : Main/Zipline.hs Tree/Zipline.hs
	ghc -o ~/bin/zipline Main/Zipline.hs

peel : Main/Peel.hs Monad/Parser.hs Pgn/Parser.hs
	ghc -o ~/bin/peel Main/Peel.hs

splay : Main/Splay.hs Tree/Splay.hs Tree/Peel.hs Utils.hs 
	ghc -o ~/bin/splay Main/Splay.hs

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

board : Chess/Init.hs Chess/Board.hs Chess/Battle.hs Monad/Parser.hs Utils.hs Chess/Move.hs
	ghc Chess/Init.hs

lint : 
	hlint -i"Use lambda-case" -i"Use String" .

clean :
	find . -name '*.hi' -delete
	find . -name '*.o' -delete
