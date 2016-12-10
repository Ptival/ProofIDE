.PHONY: all

# TODO: Make this better once it builds
all: proofide.nix Lexer.hs Parser.hs
	ghcjs Main.hs

Lexer.hs: Lexer.x
	alex $<

Parser.hs: Parser.y
	happy $<

proofide.nix: proofide.cabal
	cabal2nix . > $@
