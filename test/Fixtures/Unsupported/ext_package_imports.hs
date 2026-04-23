-- Gap: `PackageImports` — `import "pkg-name" Module`. Seen in: ihp-schema-compiler/IHP/SchemaCompiler.hs:20 (`import "interpolate" Data.String.Interpolate`). Ref: ihp-unsupported-scan.md.
{-# LANGUAGE PackageImports #-}
import "base" Data.List (sort)

main = print (sort [3, 1, 2])
