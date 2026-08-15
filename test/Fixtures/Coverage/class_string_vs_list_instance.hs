-- [Char] must pick `instance C String`, not a colliding `instance C [T]`
-- that shares the runtime [] tag.  Same leftover as blaze toHtml/toMarkup
-- on String vs ToMarkup [Markup].
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

class ToM a where
    toM :: a -> String

instance ToM String where
    toM s = "str:" ++ s

instance ToM [Int] where
    toM _ = "ints"

main :: IO ()
main = putStrLn (toM ("hello" :: String))
