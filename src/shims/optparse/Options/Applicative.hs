module Options.Applicative
    ( Parser
    , ParserInfo
    , execParser
    , info
    , helper
    , fullDesc
    , progDesc
    , header
    , (<**>)
    ) where

newtype Parser a = Parser a
newtype ParserInfo a = ParserInfo a
newtype Mod f a = Mod ()

instance Functor Parser where
    fmap f (Parser x) = Parser (f x)

instance Applicative Parser where
    pure = Parser
    Parser f <*> Parser x = Parser (f x)

execParser :: ParserInfo a -> IO a
execParser (ParserInfo x) = pure x

info :: Parser a -> Mod f a -> ParserInfo a
info (Parser x) _ = ParserInfo x

helper :: Parser (a -> a)
helper = Parser id

fullDesc :: Mod f a
fullDesc = Mod ()

progDesc :: String -> Mod f a
progDesc _ = Mod ()

header :: String -> Mod f a
header _ = Mod ()

(<**>) :: Parser a -> Parser (a -> b) -> Parser b
Parser x <**> Parser f = Parser (f x)
