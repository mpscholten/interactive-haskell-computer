{
  description = "Interactive Haskell Computer — a demand-driven, single-pass, copy-and-patch JIT interpreter for Haskell on macOS/aarch64";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghc = pkgs.haskell.compiler.ghc910;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            ghc
            pkgs.cabal-install
            pkgs.haskellPackages.hlint
            pkgs.haskellPackages.ormolu
            pkgs.haskellPackages.haskell-language-server
            pkgs.pkg-config
          ];

          shellHook = ''
            # Only show the banner on interactive shells so `nix develop -c …`
            # command substitutions ($()) stay clean.
            if [ -t 1 ]; then
              echo "IHC dev shell — GHC $(ghc --version), cabal $(cabal --version | head -1)" >&2
              echo "Target: macOS / aarch64 only." >&2
              if [ "$(uname -sm)" != "Darwin arm64" ]; then
                echo "WARNING: not on Darwin arm64 — builds will fail (MAP_JIT + Apple-ABI stencils)." >&2
              fi
            fi

            # Auto-populate the ihc interpreter source cache with common
            # Haskell libraries so `:l` / `run` can source-load them
            # without the user having to cabal-get manually each time.
            # Best-effort: silent, parallel, skipped per-package if cached.
            export IHC_CACHE="$HOME/.cache/ihc/sources"
            mkdir -p "$IHC_CACHE"
            _ihc_prefetch () {
              local pkg="$1"
              if ! ls -d "$IHC_CACHE/$pkg"-* >/dev/null 2>&1; then
                (cd "$IHC_CACHE" && cabal get "$pkg" >/dev/null 2>&1) || true
              fi
            }
            if [ -t 1 ]; then
              for p in hspec hspec-core hspec-discover call-stack HUnit \
                       QuickCheck splitmix random tf-random \
                       mtl transformers containers text bytestring; do
                _ihc_prefetch "$p" &
              done
              wait
            fi
          '';
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
