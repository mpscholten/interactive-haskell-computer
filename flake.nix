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
          directory
          filepath
          process
          unix
          time
          deepseq
          exceptions
          stm
          async
          hashable
          unordered-containers
          vector
          scientific
          attoparsec
          # network: ships .hsc sources and cbits/*.c.  Adding it here
          # lets ihcSourceRootWithHsc run ./configure (to generate
          # include/HsNetworkConfig.h) and then hsc2hs every *.hsc,
          # producing the .hs shadows the interpreter needs for
          # Network.Socket.*.  The cbits/HsNet.c + cbits/cmsg.c side
          # is handled by the separate libhsnetDylib derivation below.
          network
        ];

        # Keep only packages that actually have a derivation and are not
        # marked broken.  (Boot-lib slots are null; some Hackage packages
        # may be broken for a given GHC snapshot.)
        ihcHackageSources = pkgs.lib.filter
          (p: p != null && !(p.meta.broken or false))
          ihcHackageSourceCandidates;

        # GHC ships the source of every boot library under libraries/<pkg>/.
        # Some libs have a nested layout: libraries/containers/containers/containers.cabal.
        # We extract each one and place it under $out/<pkg>-<version>/ so the
        # IHC loader finds it alongside the Hackage tarballs.
        # "base" is intentionally excluded — it requires special treatment and
        # is already handled via ~/.cache/ihc/sources/base-*/.
        ghcSrc = pkgs.haskell.compiler.ghc910.src;

        ghcBootLibs = [
          "directory"
          "filepath"
          "process"
          "time"
          "deepseq"
          "stm"
          "unix"
          "exceptions"
          "containers"
          "mtl"
          "transformers"
          "text"
          "bytestring"
        ];

        # A single flat directory containing every .h file shipped with the
        # installed GHC (rts, ghc-internal, bytestring, unix, time, process,
        # ghc-bignum, …).  This lets IHC's CPP preprocessor resolve system
        # headers like MachDeps.h, HsFFI.h, HsBaseConfig.h that live in the
        # GHC installation but not in Hackage source tarballs.
        #
        # We collect headers from all per-package include/ subdirs under
        #   <ghc>/lib/ghc-<ver>/lib/<platform>-ghc-<ver>/<pkg>-<ver>/include/
        # as well as the top-level rts include tree (which contains MachDeps.h,
        # DerivedConstants.h, ghcplatform.h, HsFFI.h, Rts.h, etc.).
        # Sub-directories (rts/, stg/) are preserved by using cp -r so that
        # includes like #include "rts/Types.h" also resolve.
        ghcIncludeDirs = pkgs.runCommand "ihc-ghc-includes" { } ''
          mkdir -p $out
          # Walk every include/ subdir under the installed GHC package tree
          # and copy its contents into the flat output dir.  cp -rn (no-clobber)
          # means earlier (more important) headers win when names collide.
          for inc_dir in ${ghc}/lib/ghc-*/lib/*-ghc-*/*/include; do
            if [ -d "$inc_dir" ]; then
              cp -rn "$inc_dir"/. $out/ 2>/dev/null || true
            fi
          done
        '';

        # Derivation that copies each boot-lib source tree from GHC's source
        # into $out/<pkg>-<version>/.  Version is read from the .cabal file
        # using a case-insensitive grep so it handles both "version:" and
        # "Version:" spellings.
        ghcBootSourceRoot = pkgs.runCommand "ihc-ghc-boot-libs" { } ''
          mkdir -p $out
          ${pkgs.lib.concatMapStringsSep "\n" (pkg: ''
            if [ -d "${ghcSrc}/libraries/${pkg}" ]; then
              cabal_file=$(find "${ghcSrc}/libraries/${pkg}" -maxdepth 3 -name "${pkg}.cabal" -type f | head -1)
              if [ -n "$cabal_file" ]; then
                version=$(grep -im1 '^version:' "$cabal_file" | awk '{print $2}' | tr -d '\r')
                src_dir=$(dirname "$cabal_file")
                target="$out/${pkg}-$version"
                cp -r "$src_dir" "$target"
                chmod -R u+w "$target"
              fi
            fi
          '') ghcBootLibs}
        '';

        # A single derivation that unpacks every source tarball into $out.
        # Result layout: $out/hspec-2.11.16/, $out/QuickCheck-2.15.0.1/, …
        # GHC boot-lib sources are merged in from ghcBootSourceRoot.
        # The loader (IHC.CabalProject.cachedPackageSearchPath) can then
        # enumerate $out the same way it enumerates ~/.cache/ihc/sources/.
        ihcSourceRoot = pkgs.runCommand "ihc-hackage-sources" { } ''
          mkdir -p $out
          ${pkgs.lib.concatMapStringsSep "\n" (p:
            "${pkgs.gnutar}/bin/tar -xf ${p.src} -C $out"
          ) ihcHackageSources}
          # GHC boot libs — plain directories, not tarballs
          cp -r ${ghcBootSourceRoot}/* $out/ 2>/dev/null || true
          chmod -R u+w $out
        '';

        # Post-process: walk $ihcSourceRoot, for each *.hsc generate the
        # sibling *.hs via hsc2hs.  This makes packages like directory, unix,
        # time, and process interpretable by IHC's source loader which only
        # looks for .hs files.
        #
        # Strategy:
        #  1. Copy the source root into $out (writable).
        #  2. For every package that ships a configure script, run it first so
        #     that autoconf-generated headers (HsDirectoryConfig.h,
        #     HsUnixConfig.h, HsTimeConfig.h, HsProcessConfig.h, …) exist
        #     before hsc2hs tries to include them.
        #  3. Run hsc2hs on every *.hsc.  Failures are logged but do NOT abort
        #     the build — absence of a .hs just means the loader will fail
        #     cleanly for that specific module later.
        #
        # Include search order for hsc2hs:
        #   a. ${ghcIncludeDirs}  — flat merged GHC+RTS headers (HsFFI.h etc.)
        #   b. $pkgroot/include   — per-package headers generated by configure
        #   c. $pkgroot/lib/include — time-style nested include tree
        #   d. $pkgroot           — for packages that generate the header in the
        #                           root dir (directory generates HsDirectoryConfig.h
        #                           directly in the package root)
        #
        # MIN_VERSION_* macros: hsc2hs invokes the C pre-processor without
        # Cabal's version-macro header.  We inject a blanket definition for the
        # packages we know need it.  base-4.20.2.0 ships with GHC 9.10.
        ihcSourceRootWithHsc = pkgs.runCommand "ihc-hackage-sources-hsc" {
          # GHC provides hsc2hs; stdenv.cc provides the C compiler (clang on Darwin)
          # that hsc2hs invokes to evaluate #size/#peek/#const expressions.
          buildInputs = [ ghc pkgs.stdenv.cc ];
        } ''
          # ── 1. Copy source tree ─────────────────────────────────────────────
          cp -r ${ihcSourceRoot}/. "$out"
          chmod -R u+w "$out"

          hsc2hs_bin="${ghc}/bin/hsc2hs"
          ghc_incs="${ghcIncludeDirs}"

          # Version macros for packages shipped with GHC 9.10.
          # MIN_VERSION_X(a,b,c) is true iff package X >= (a,b,c).
          # base-4.20.2.0, filepath-1.5.4.0 ship with GHC 9.10.
          min_ver_defs=""
          min_ver_defs="$min_ver_defs -DMIN_VERSION_base(a,b,c)=(((a)<4)||((a)==4&&(b)<20)||((a)==4&&(b)==20&&(c)<=2))"
          min_ver_defs="$min_ver_defs -DMIN_VERSION_filepath(a,b,c)=(((a)<1)||((a)==1&&(b)<5)||((a)==1&&(b)==5&&(c)<=4))"

          # ── 2. Run ./configure in packages that have one ────────────────────
          # Some tarballs (e.g. network-3.2.8.0) ship configure without the
          # executable bit set, so `./configure` would fail with "permission
          # denied" before autoconf's own logic runs.  chmod +x defensively.
          # configure's stderr is captured to a log file rather than
          # discarded — silent failures were making hsc2hs breakage
          # very hard to diagnose.
          for pkg_dir in "$out"/*/; do
            [ -f "$pkg_dir/configure" ] || continue
            (
              cd "$pkg_dir"
              chmod +x ./configure 2>/dev/null || true
              echo "[hsc2hs-prep] running ./configure in $pkg_dir" >&2
              conf_log="$(mktemp)"
              if ./configure >"$conf_log" 2>&1; then
                :
              else
                echo "[hsc2hs-prep] WARNING: configure failed in $pkg_dir" >&2
                tail -20 "$conf_log" >&2
              fi
              rm -f "$conf_log"
            )
          done

          # ── 3. Process every .hsc file ──────────────────────────────────────
          ok=0; fail=0
          while IFS= read -r -d "" hsc; do
            ouths="''${hsc%.hsc}.hs"
            [ -f "$ouths" ] && continue          # already generated, skip

            # The package root is always the direct child of $out
            # (layout: $out/<pkg>-<ver>/path/to/Foo.hsc)
            rel="''${hsc#"$out/"}"         # strip leading $out/
            pkgname="''${rel%%/*}"         # first path component = pkg-ver dir
            pkgroot="$out/$pkgname"

            # Collect per-package include dirs
            inc_flags="-I$pkgroot"
            [ -d "$pkgroot/include"     ] && inc_flags="$inc_flags -I$pkgroot/include"
            [ -d "$pkgroot/lib/include" ] && inc_flags="$inc_flags -I$pkgroot/lib/include"

            err_log="$(mktemp)"
            if "$hsc2hs_bin" \
                -I"$ghc_incs" $inc_flags \
                $min_ver_defs \
                "$hsc" -o "$ouths" \
                2>"$err_log"; then
              ok=$((ok+1))
            else
              fail=$((fail+1))
              echo "[hsc2hs] FAILED: $hsc" >&2
              cat "$err_log" >&2
            fi
            rm -f "$err_log"
          done < <(find "$out" -name '*.hsc' -print0)

          echo "[hsc2hs] done: $ok converted, $fail failed" >&2
        '';

        # ────────────────────────────────────────────────────────────────
        # ihcCbitsRoot — auto-discovered per-package cbits dylibs.
        #
        # Scans every package directory under ihcSourceRootWithHsc for a
        # cbits/ subdir, discovers every .c source file under it, and
        # compiles them into @lib<pkg>-cbits.dylib@.  Rather than parsing
        # cabal's @c-sources:@ (which is tangled in @if arch()@/@os()@
        # conditionals that vary per package version), we use a simpler
        # heuristic that works for every package tested so far:
        #   * Compile every .c under cbits/ individually.
        #   * Prefer arch-specific variants (cbits/aarch64/foo.c beats
        #     cbits/foo.c) so symbols aren't defined twice.
        #   * Skip files whose compile fails (Windows-only helpers,
        #     flag-gated C++ simdutf, etc.).
        # Output layout: $out/lib/lib<pkg>-cbits.dylib per package that
        # produced at least one object file.  Per-package build logs are
        # kept at $out/lib/<pkg>.err for diagnosis.
        ihcCbitsRoot = pkgs.runCommand "ihc-cbits-dylibs" {
          buildInputs = [ pkgs.stdenv.cc ];
        } ''
          mkdir -p $out/lib

          # GHC headers (HsFFI.h, HsBaseConfig.h, …) live outside the
          # Hackage tarballs; expose them so package cbits that include
          # <HsFFI.h> build.
          ghc_inc=""
          for d in ${ghcIncludeDirs}; do
            if [ -d "$d" ]; then ghc_inc="$ghc_inc -I$d"; fi
          done
          # Fallback: crawl GHC's own per-package include dirs.
          for d in ${ghc}/lib/ghc-*/lib/*-ghc-*/*/include; do
            if [ -d "$d" ]; then ghc_inc="$ghc_inc -I$d"; fi
          done

          shopt -s nullglob
          for pkg_dir in "${ihcSourceRootWithHsc}"/*/; do
            pkg_dir="''${pkg_dir%/}"
            pkg=$(basename "$pkg_dir")
            if [ ! -d "$pkg_dir/cbits" ]; then
              continue
            fi

            # Package-local -I flags.
            incs=""
            [ -d "$pkg_dir/include" ]     && incs="$incs -I$pkg_dir/include"
            [ -d "$pkg_dir/cbits" ]       && incs="$incs -I$pkg_dir/cbits"
            [ -d "$pkg_dir/cbits/include" ] && incs="$incs -I$pkg_dir/cbits/include"

            # Discover all .c files under cbits/.  Give arch-specific
            # variants priority over generic ones (keep aarch64/foo.c,
            # drop cbits/foo.c if the same basename exists under
            # aarch64/).
            tmp_list=$(mktemp)
            find "$pkg_dir/cbits" -name '*.c' -print > "$tmp_list"

            # Build a basename→path map preferring aarch64/ paths.
            declare -A chosen
            while IFS= read -r f; do
              base=$(basename "$f")
              if [ -n "''${chosen[$base]:-}" ]; then
                case "$f" in *aarch64*) chosen[$base]="$f" ;; esac
              else
                chosen[$base]="$f"
              fi
            done < "$tmp_list"
            rm "$tmp_list"

            # Compile each selected .c to an object file; collect
            # successes, skip failures.
            work=$(mktemp -d)
            objs=""
            log="$out/lib/$pkg.err"
            : > "$log"
            for f in "''${chosen[@]}"; do
              obj="$work/$(basename "$f" .c).o"
              if cc -fPIC -c -O $ghc_inc $incs -o "$obj" "$f" 2>>"$log"; then
                objs="$objs $obj"
              else
                echo "  SKIP $f (see $log)" >> "$log"
              fi
            done
            unset chosen

            if [ -z "$objs" ]; then
              echo "cbits: SKIPPED $pkg (no compilable .c files; see $log)" >&2
              continue
            fi

            out_dylib="$out/lib/lib$pkg-cbits.dylib"
            if cc -fPIC -shared -O -undefined dynamic_lookup \
                  -o "$out_dylib" $objs 2>>"$log"; then
              rm -f "$log"
              echo "cbits: built $out_dylib" >&2
            else
              rm -f "$out_dylib"
              echo "cbits: LINK FAILED $pkg (see $log)" >&2
            fi
          done
        '';

      in {
        # Expose as a package so `nix build .#cbits` works for debugging.
        packages.cbits = ihcCbitsRoot;
        devShells.default = pkgs.mkShell {
          buildInputs = [
            ghc
            pkgs.cabal-install
            hp.hlint
            hp.ormolu
            hp.haskell-language-server
            pkgs.pkg-config
            pkgs.libffi
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
            # ihcSourceRootWithHsc extends ihcSourceRoot by pre-processing every
            # *.hsc file via hsc2hs so the interpreter sees plain *.hs files.
            export IHC_NIX_SOURCE_DIR="${ihcSourceRootWithHsc}"

            # GHC's own header files (MachDeps.h, HsFFI.h, HsBaseConfig.h, …)
            # live in the installed GHC package tree, not in Hackage tarballs.
            # IHC.Cpp reads this colon-separated list as a last-resort include
            # search path so that #include "MachDeps.h" resolves correctly.
            export IHC_GHC_INCLUDE_DIRS="${ghcIncludeDirs}"

            # ~/.cache/ihc/sources/ remains available for packages NOT in the
            # nix bundle above; users can still `cabal get <pkg>` there as
            # needed.  We no longer auto-prefetch on shell entry.
            export IHC_CACHE="$HOME/.cache/ihc/sources"
            mkdir -p "$IHC_CACHE"

            # libhsnet.dylib — native cbits for the `network` package.
            # Expose its directory on DYLD_LIBRARY_PATH (and the Apple
            # fallback equivalent) so that a later dlopen("libhsnet.dylib")
            # from the FFI dispatcher can resolve without an absolute path.
            # IHC_LIBHSNET_DIR is also set so future code can reference the
            # exact store path directly.
            # Auto-discovered per-package cbits dylibs: one
            # lib<pkg>-cbits.dylib per Hackage package that declares
            # @c-sources:@ in its cabal.  FFI.registerCbitsDylibs at
            # interpreter startup dlopens every *.dylib in this dir.
            export IHC_CBITS_DIR="${ihcCbitsRoot}/lib"
            export DYLD_LIBRARY_PATH="$IHC_CBITS_DIR''${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
            export DYLD_FALLBACK_LIBRARY_PATH="$IHC_CBITS_DIR''${DYLD_FALLBACK_LIBRARY_PATH:+:$DYLD_FALLBACK_LIBRARY_PATH}"
          '';
        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
