.PHONY: build clean repl watch ;\
	test unit integration functional ;\
	cic ci formatc format lint lintc ;\
	haddock haddockc hackage

# dev

ARGS = ""

build:
	if [ -z "$(ARGS)" ]; then \
		cabal build; \
	else \
		cabal build $(ARGS); \
	fi

clean:
	cabal clean

repl:
	if [ -z "$(ARGS)" ]; then \
		cabal repl safe-rm; \
	else \
		cabal repl $(ARGS); \
	fi

watch:
	if [ -z "$(ARGS)" ]; then \
		ghcid --command "cabal repl safe-rm"; \
	else \
		ghcid --command "cabal repl $(ARGS)"; \
	fi

# testing

test:
	if [ -z "$(ARGS)" ]; then \
		cabal test; \
	else \
		cabal test $(ARGS); \
	fi

unit:
	cabal test unit

integration:
	cabal test integration

functional:
	cabal test functional

# ci

cic: formatc lintc haddockc

ci: lint format haddockc

# formatting

formatc:
	nix run github:tbidne/nix-hs-tools/0.6.1#nixpkgs-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.6.1#cabal-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.6.1#ormolu -- --mode check

format:
	nix run github:tbidne/nix-hs-tools/0.6.1#nixpkgs-fmt ;\
	nix run github:tbidne/nix-hs-tools/0.6.1#cabal-fmt -- --inplace ;\
	nix run github:tbidne/nix-hs-tools/0.6.1#ormolu -- --mode inplace

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.6.1#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.6.1#hlint

# generate docs for main package, copy to docs/
haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.4/safe-rm-0.1/opt/doc/html/safe-rm/* docs/

haddockc:
	nix run github:tbidne/nix-hs-tools/0.6.1#haddock-cov -- \
	. \
	-m SafeRm.Prelude 95 \
	-m SafeRm.Runner.Command 20 \
	-m SafeRm.Runner.Args 50

# generate dist and docs suitable for hackage
hackage:
	cabal sdist ;\
	cabal haddock --haddock-for-hackage --enable-doc
