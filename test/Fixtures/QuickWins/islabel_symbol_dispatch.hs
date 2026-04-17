-- Phase 3.5: IHP-shaped IsLabel instance with Symbol-literal class parameter.
-- `instance IsLabel "email" Wrap where ...` should dispatch distinctly from
-- `instance IsLabel "name" Wrap where ...` — the label's Symbol is part of
-- the registry key so each #field selects the right method body.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
import GHC.OverloadedLabels (IsLabel(..))

data Wrap = Wrap String deriving Show

instance IsLabel "email" Wrap where
    fromLabel = Wrap "from-email"

instance IsLabel "name" Wrap where
    fromLabel = Wrap "from-name"

main = do
    print (#email :: Wrap)
    print (#name :: Wrap)
