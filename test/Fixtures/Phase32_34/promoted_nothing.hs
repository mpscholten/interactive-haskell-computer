-- Phase 3.4: promoted constructors 'Nothing, 'Just in type sigs (parse-discard)
-- The tick+constructor in a type sig is lexed as char+ident (harmless),
-- but since we discard type sigs the value-level function runs normally.
{-# LANGUAGE DataKinds #-}

module Main (main) where

-- Type sig uses promoted 'Nothing — interpreter discards it.
-- Value-level code: just an Int function.
answer :: Int
answer = 99

main :: IO ()
main = print answer
