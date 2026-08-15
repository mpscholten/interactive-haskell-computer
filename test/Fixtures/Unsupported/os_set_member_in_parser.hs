{-# LANGUAGE OverloadedStrings #-}
-- Gap: Set.fromList ["h1"] :: Set Text stays [Char]; member inside
-- ParsecT.  Reduced parseHsx leftover after quoteExp field force.
-- Reduced parseHsx leftover: Set.fromList of OverloadedStrings
-- literals (`parents :: Set Text`) plus Set.member of a takeWhile1P
-- Text name inside ParsecT.  Same IhcException: ParsecT as used
-- IHP.HSX.Parser.parseHsx on "<h1>…".  No IHP import.
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

main = do
    putStrLn "start"
    case parse p "" (T.pack "<h1>Hello") of
        Left _  -> putStrLn "LEFT"
        Right t -> putStrLn (T.unpack t)
