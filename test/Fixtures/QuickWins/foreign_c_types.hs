-- Foreign.C.Types is ordinary base source (.hs in modern base-4.20.2.0
-- that ihc uses from ~/.cache/ihc/sources/; the historically .hsc
-- layout was flattened before this base vendoring).  The flake's
-- hsc2hs preprocessing step (flake.nix ihcSourceRootWithHsc) handles
-- any residual .hsc files in the nix-pinned source tree, and the
-- handful of .hsc-only modules in base/ghc-internal that have no .hs
-- sibling are Windows-only.  Source-backed modules such as
-- GHC.RTS.Flags must still route through the preprocessed .hs files.
--
-- This fixture locks in a round-trip of CInt as an ordinary import so
-- a regression — e.g. a .hsc resurfacing to the scheduler because the
-- flake preprocessing stops covering it, or base bumping the layout
-- back to .hsc — trips immediately instead of silently at library-
-- load time.
import Foreign.C.Types (CInt)

main :: IO ()
main = print (42 :: CInt)
