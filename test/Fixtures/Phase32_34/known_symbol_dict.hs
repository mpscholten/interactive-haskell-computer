-- Phase 3.2 + 3.4: KnownSymbol class-dict dispatch.
-- symbolVal p where p :: Proxy s and s is constrained by KnownSymbol s =>
-- The class dict is threaded through the proxy at call site.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Proxy
import GHC.TypeLits

foo :: KnownSymbol s => Proxy s -> String
foo p = symbolVal p

main :: IO ()
main = putStrLn (foo (Proxy :: Proxy "hello"))
