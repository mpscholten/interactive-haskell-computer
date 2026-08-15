-- Gap: used-parseHsx hang. OS Set.fromList ["h1"] :: Set Text +
-- Set.member is isolated in Coverage/set_fromlist_overloaded_strings_member
-- (defining-module Internal). This file keeps the megaparsec wrapper
-- IHP.HSX.Parser uses (takeWhile1P then Set.member parents). T.pack set
-- is a different leftover (int-spine Num.+). Facade Data.Set + Text
-- still lazy-rebuilds Internal.
{-# LANGUAGE OverloadedStrings #-}
import Text.Megaparsec
import Text.Megaparsec.Char (char, space)
import Data.Void
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Char as Char

type Parser = Parsec Void T.Text

parents :: Set.Set T.Text
parents = Set.fromList ["a", "div", "h1", "h2", "span", "p"]

p :: Parser T.Text
p = do
    _ <- char '<'
    name <- takeWhile1P (Just "identifier") Char.isAlphaNum
    let ok = name `Set.member` parents
    if ok then pure () else fail "bad tag"
    space
    _ <- char '>'
    pure name

main = case parse p "" (T.pack "<h1>Hello") of
    Left _  -> putStrLn "LEFT"
    Right t -> putStrLn (T.unpack t)
