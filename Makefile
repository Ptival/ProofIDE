.PHONY: all clean

GHCJS=ghc

# TODO: Make this better once it builds
all: proofide.nix Main.jsexe

Main.jsexe: Main.hs Lexer.hs Parser.hs
	$(GHCJS) Main.hs

Lexer.hs: Lexer.x
	alex $<

Parser.hs: Parser.y
	happy $<

proofide.nix: proofide.cabal
	cabal2nix . > $@

clean:
	rm Lexer.hs Parser.hs
	rm *.js_hi *.js_o
	rm proofide.nix
