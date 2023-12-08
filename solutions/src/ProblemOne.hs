module ProblemOne
    ( someFunc
    ) where

import Parsers
import Data.Char
import FileRead
import Data.Monoid
import Control.Applicative

someFunc :: IO ()
someFunc = do
    lines <- readSampleFile "\\src\\SampleInput\\ProblemOne.txt"

    print $ sum (parseEachLine lines)



--matchSingleLine [] _ = [""]

parseEachLine :: [String] -> [Int]
parseEachLine (x:lines) = ([readFirstAndLast x []] ++ parseEachLine lines)
parseEachLine [] = []

readFirstAndLast :: [Char] -> [Char] -> Int
readFirstAndLast [] ys =  read ([(head ys)] ++ [(last ys)]) :: Int
readFirstAndLast (x:input)  ys  
                                | isToken(x:input) = readFirstAndLast input (transformInput (x:input))
                                | isNumber(x) = readFirstAndLast input (ys ++ [x])
                                | otherwise = readFirstAndLast input ys
                                where
                                    transformInput line = case matchNumberWords line of
                                        Left err -> ""
                                        Right (match, rest) -> ys ++ [transformWordToNumber match]


transformWordToNumber :: [Char] -> Char
transformWordToNumber word = case word of
                                "one" -> '1'
                                "two" -> '2'
                                "three" -> '3'
                                "four" -> '4'
                                "five" ->'5'
                                "six" -> '6'
                                "seven" -> '7'
                                "eight" -> '8'
                                "nine" -> '9'
                                _ -> '0'

isToken :: [Char] -> Bool
isToken line = case matchNumberWords line of
                 Left err -> False
                 Right (_, _) -> True

matchOne :: Parser Char [Char]
matchOne = (sequenceA $ map matchChar "one")

matchTwo :: Parser Char [Char]
matchTwo = (sequenceA $ map matchChar "two")

matchThree :: Parser Char [Char]
matchThree = (sequenceA $ map matchChar "three")

matchFour :: Parser Char [Char]
matchFour = (sequenceA $ map matchChar "four")

matchFive :: Parser Char [Char]
matchFive = (sequenceA $ map matchChar "five")

matchSix :: Parser Char [Char]
matchSix = (sequenceA $ map matchChar "six")

matchSeven :: Parser Char [Char]
matchSeven = (sequenceA $ map matchChar "seven")

matchEight :: Parser Char [Char]
matchEight = (sequenceA $ map matchChar "eight")

matchNine :: Parser Char [Char]
matchNine = (sequenceA $ map matchChar "nine")

matchNumberWords :: String -> Either [Error] ([Char], [Char])
matchNumberWords s = parse (matchOne <|> 
                            matchTwo <|>
                            matchThree <|>
                            matchFour <|>
                            matchFive <|>
                            matchSix <|>
                            matchSeven <|>
                            matchEight <|>
                            matchNine )
                            s