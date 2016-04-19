all: Interpreter

Lexsyntax.x : syntax.cf
	bnfc -haskell syntax.cf

Lexsyntax.hs : Lexsyntax.x
	alex Lexsyntax.x

Parsyntax.hs : Parsyntax.y
	happy Parsyntax.y

Interpreter: Lexsyntax.hs Parsyntax.hs Interpreter.hs
	ghc --make Interpreter

clean:
	rm -f Interpreter \
		  Lexsyntax.x \
		  Parsyntax.y \
		  Abssyntax.hs \
		  Parsyntax.y \
		  Docsyntax.tex \
		  Docsyntax.txt \
		  Skelsyntax.hs \
		  Printsyntax.hs \
		  Testsyntax.hs \
		  ErrM.hs \
		  Parsyntax.hs \
		  Lexsyntax.hs \
		  *.o \
		  *.hi

