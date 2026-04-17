-- Phase 3.2 + 3.4: KnownNat class-dict dispatch.
-- natVal p where p :: Proxy n and n is constrained by KnownNat n =>
-- The class dict is threaded through the proxy at call site: the
-- (Proxy :: Proxy 42) annotation attaches VInt 42 payload which natVal reads.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Proxy
import GHC.TypeLits

bar :: KnownNat n => Proxy n -> Integer
bar p = natVal p

main :: IO ()
main = print (bar (Proxy :: Proxy 42))
