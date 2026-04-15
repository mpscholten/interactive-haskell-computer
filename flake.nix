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
          '';
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
