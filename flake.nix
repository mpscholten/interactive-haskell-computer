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
        # Use the same GHC set as the shell so package sources match the
        # compiler version that ihc targets.
        hp = pkgs.haskell.packages.ghc910;
        ghc = pkgs.haskell.compiler.ghc910;

        # Haskell packages whose sources ihc should be able to interpret.
        # GHC boot libraries (mtl, transformers, containers, text, bytestring,
        # etc.) are null in the ghc910 Haskell package set — they ship with
        # GHC itself and have no separate derivation here.  We filter them out
        # with the `p != null` guard below and rely on the ihc source cache
        # for any that the user separately provides.
        ihcHackageSourceCandidates = with hp; [
          hspec
          hspec-core
          hspec-discover
          hspec-expectations
          QuickCheck
          splitmix
          random
          tf-random
          call-stack
          HUnit
          aeson
        ];

        # Keep only packages that actually have a derivation and are not
        # marked broken.  (Boot-lib slots are null; some Hackage packages
        # may be broken for a given GHC snapshot.)
        ihcHackageSources = pkgs.lib.filter
          (p: p != null && !(p.meta.broken or false))
          ihcHackageSourceCandidates;

        # A single derivation that unpacks every source tarball into $out.
        # Result layout: $out/hspec-2.11.16/, $out/QuickCheck-2.15.0.1/, …
        # The loader (IHC.CabalProject.cachedPackageSearchPath) can then
        # enumerate $out the same way it enumerates ~/.cache/ihc/sources/.
        ihcSourceRoot = pkgs.runCommand "ihc-hackage-sources" { } ''
          mkdir -p $out
          ${pkgs.lib.concatMapStringsSep "\n" (p:
            "${pkgs.gnutar}/bin/tar -xf ${p.src} -C $out"
          ) ihcHackageSources}
        '';
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            ghc
            pkgs.cabal-install
            hp.hlint
            hp.ormolu
            hp.haskell-language-server
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

            # Nix-pinned Hackage source tree — always available, no network
            # required.  IHC.CabalProject.cachedPackageSearchPath enumerates
            # this directory alongside ~/.cache/ihc/sources/.
            export IHC_NIX_SOURCE_DIR="${ihcSourceRoot}"

            # ~/.cache/ihc/sources/ remains available for packages NOT in the
            # nix bundle above; users can still `cabal get <pkg>` there as
            # needed.  We no longer auto-prefetch on shell entry.
            export IHC_CACHE="$HOME/.cache/ihc/sources"
            mkdir -p "$IHC_CACHE"
          '';
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
