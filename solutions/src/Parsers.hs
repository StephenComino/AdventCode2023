{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Parsers
    ( matchWord,
      matchChar,
      satisfy,
            Error,
            parse,
            Parser
    ) where

import Data.Monoid
import Control.Applicative

data Error = Fail deriving Show
data Tokens = START | MIDDLE | END deriving (Show)

newtype Parser a i = Parser {
    parse :: [a] -> Either [Error] (i, [a])
}

instance Show (Parser a [a]) where
    show (Parser l) = "Parser"

instance Semigroup a => Semigroup (Parser a a) where
    (Parser a) <> (Parser b) = Parser $ \input -> 
        case a input of
            Left err -> Left err
            Right (x,xs) -> 
                case b xs of
                    Left err -> Left err
                    Right (y,ys) -> Right (x <> y, ys)

instance (Monoid a) => Monoid (Parser a a) where
    mempty = Parser $ \input -> Left [Fail]

-- Functor: Apply a function to the result of the DataType
-- How can we map over Parser
instance Functor (Parser a) where
    fmap f (Parser p) = Parser $ \input -> 
        case p input of
            Left err -> Left err
            Right (inp, rest) -> Right (f inp, rest)


-- Applicative: Perform two Consecutive Parsers
instance Applicative (Parser a) where
    pure a = Parser $ \input -> Right (a, input)
    Parser a <*> Parser b = Parser $ \input -> 
        case a input of
            Left err -> Left err
            Right (output, rest) -> 
                case b rest of
                    Left err -> Left err
                    Right (o, r) -> Right (output o, r)

instance Alternative (Parser a) where
    empty = Parser $ \input -> Left [Fail]
    Parser a <|> Parser b = Parser $ \input ->
        case a input of
            Left err -> 
                case b input of
                    Left err -> Left err
                    Right (x,xs) -> Right (x,xs)
            Right (x,xs) -> Right (x,xs)

instance Monad (Parser a) where
    Parser p >>= k = Parser $ \input ->
        case p input of
        Left err -> Left err
        Right (output, rest) ->
            let
                Parser p' = k output
            in
                p' rest
    
    --p >>= f = Parser $ \input ->
    --    case p input of
    --        Left err -> Left err
    --        Right (x, xs) ->
    --            case f xs of
    --                Left err -> Left err
    --                Right (x )



-- Generic functions which takes a predicate and
-- Matches on Input
satisfy :: (a -> Bool) -> Parser a a
satisfy pred = Parser $ \input ->
    case input of
        [] -> Left [Fail]
        hd : rest
            | pred hd -> Right (hd, rest)
            | otherwise -> Left [Fail]

matchChar :: (Eq a) => a -> Parser a a
matchChar x = Parser $ \input ->
    case input of
        [] -> Left [Fail]
        (y:xs) 
            | y == x    -> Right (x,xs)
            | otherwise -> Left [Fail] 

matchWord :: String -> Either [Error] ([Char], [Char])
matchWord s = parse (sequenceA $ map matchChar "word") s

parseBeginning :: Parser Char Tokens
parseBeginning = START <$ (sequenceA $ map matchChar "start")
