.PHONY: all

proofide.nix: proofide.cabal
	cabal2nix . > $@

# TODO: Make this better once it builds
all: proofide.nix
	ghcjs Main.hs
