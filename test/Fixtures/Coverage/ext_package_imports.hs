-- Coverage: `PackageImports` accepts `import "pkg-name" Module` and ignores
-- the package qualifier for module-name resolution.
{-# LANGUAGE PackageImports #-}
import "base" Data.List (sort)

main = print (sort [3, 1, 2])
