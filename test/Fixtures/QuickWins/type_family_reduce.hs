-- | Tier-1 IHP blocker: runtime reduction of type-family applications
-- used by @symbolVal@.  Without the reducer, @symbolVal \@(TableNameOf
-- User)@ would extract the raw identifier bytes; with it, the ETyApp
-- path in IHC.Eval rewrites @TableNameOf User@ to @"users"@ before the
-- Symbol-literal extractor runs.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import GHC.TypeLits (Symbol, symbolVal)
import Data.Proxy (Proxy(..))

type family TableNameOf model :: Symbol

data User = User
type instance TableNameOf User = "users"

data Post = Post
type instance TableNameOf Post = "posts"

main :: IO ()
main = do
    putStrLn (symbolVal (Proxy :: Proxy (TableNameOf User)))
    putStrLn (symbolVal (Proxy :: Proxy (TableNameOf Post)))
