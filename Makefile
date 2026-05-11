# Thin convenience wrapper around cabal.

CABAL ?= cabal
BUILD_DIR = dist-newstyle

.PHONY: all build test clean

all: build

build:
	$(CABAL) build all

test:
	$(CABAL) test ihc-test --test-show-details=streaming

clean:
	rm -rf $(BUILD_DIR)
