# Thin convenience wrapper around cabal + codesign. JIT pages require the
# binary to be ad-hoc signed with com.apple.security.cs.allow-jit on Apple
# Silicon; cabal has no portable post-build hook, so we do it here.

CABAL ?= cabal
BUILD_DIR = dist-newstyle

.PHONY: all build sign-ihc sign-test test check clean

all: build

build:
	$(CABAL) build all

# Find the ihc executable cabal produced and sign it in place.
sign-ihc: build
	@BIN=$$($(CABAL) list-bin exe:ihc) ; \
	 echo "signing $$BIN" ; \
	 scripts/codesign-jit.sh "$$BIN"

# Find the test binary cabal produced and sign it in place.
sign-test:
	@$(CABAL) build ihc-test
	@BIN=$$($(CABAL) list-bin test:ihc-test) ; \
	 echo "signing $$BIN" ; \
	 scripts/codesign-jit.sh "$$BIN"

test: sign-test
	$(CABAL) test ihc-test --test-show-details=streaming

check: sign-ihc
	@BIN=$$($(CABAL) list-bin exe:ihc) ; \
	 "$$BIN" --check-jit

clean:
	rm -rf $(BUILD_DIR)
