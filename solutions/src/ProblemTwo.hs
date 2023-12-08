module ProblemTwo
    ( someFunc
    ) where

import Parsers
import Data.Char
import Data.Either
import FileRead
import Data.Monoid
import Control.Applicative

someFunc :: IO ()
someFunc = do
    lines <- readSampleFile "\\SampleInput\\ProblemTwo.txt"
    print $ sum $ fmap calculateAnswer $ (fmap $ parse (parseGame 12 13 14)) lines
    --print $ (fmap $ parse (parseGame 12 13 14)) lines


calculateAnswer :: Either a (String, String) -> Int
calculateAnswer (Right ("",_)) = 0
calculateAnswer (Right (answ, _)) = (read (answ) :: Int)

parseGame :: Int -> Int -> Int -> Parser Char [Char]
parseGame red green blue = do -- red, green, blue
    first <- parseMatchBeginning
    
    rest <- many $ parseSingleGame red green blue
    --return rest
    if (elem "" rest) then do return "" else do (return first)
    


getRedValue :: Int -> Int -> Int -> Int -> [Char] -> Parser Char [Char]
getRedValue red green blue val [] = do return ""
getRedValue red green blue val (info) 
                                    | info == "red" && val > red      = do return ""
                                    | info == "blue" && val > blue    = do return ""
                                    | info == "green" && val > green  = do return ""
                                    | info == "red" && val <= red     = do return info
                                    | info == "blue" && val <= blue   = do return info
                                    | info == "green" && val <= green = do return info
                                    | otherwise                       = do return info

parseSingleGame :: Int -> Int -> Int -> Parser Char [Char]
parseSingleGame red green blue = do -- red, blue, green
    firstColon <- many $ matchSemiColon
    rest <- many $ matchSpace
    thisNumber <- many $ matchAnyNumber
    _ <- many $ matchSpace
    firstcolorName <- matchRed <|> matchBlue <|> matchGreen 
    semiColon <- many $ matchSemiColon
    result <- getRedValue red green blue (read (thisNumber) :: Int) firstcolorName
    if (result == "") then do return "" else do
        if ((length semiColon > 0) && (semiColon !! 0) == ";") then do return $ "ok" else do
            comma <- many $ matchComma

            if (length semiColon == 0) && (length comma == 0) then do return $ "ok" else do
                _ <- many $ matchSpace
                
                thisNumber <- many $ matchAnyNumber
                _ <- many $ matchSpace
                firstcolorName <- matchRed <|> matchBlue <|> matchGreen
                semiColon <- many $ matchSemiColon
                result <- getRedValue red green blue (read (thisNumber) :: Int) firstcolorName
                if (result == "") then do return "" else do
                    if ((length semiColon > 0) && (semiColon !! 0) == ";") then do return $ "ok" else do

                        comma <- many $ matchComma
                        if (length semiColon == 0) && (length comma == 0) then do return $ "ok" else do
                            _ <- many $ matchSpace

                            thisNumber <- many $ matchAnyNumber
                            _ <- many $ matchSpace
                            firstcolorName <- matchRed <|> matchBlue <|> matchGreen
                            --semiColon <- many $ matchSemiColon
                            result <- getRedValue red green blue (read (thisNumber) :: Int) firstcolorName
                            if (result == "") then do (return "")
                                else do
                                    return $ "ok"

parseMatchBeginning :: Parser Char [Char]
parseMatchBeginning = do
    first <- matchGame
    s <- many $ matchSpace
    number <- many $ matchAnyNumber
    semiColon <- matchColon
    pure $ number

matchRed :: Parser Char [Char]
matchRed = (sequenceA $ map matchChar "red")

matchBlue :: Parser Char [Char]
matchBlue = (sequenceA $ map matchChar "blue")

matchGreen :: Parser Char [Char]
matchGreen = (sequenceA $ map matchChar "green")

matchAnyNumber :: Parser Char Char
matchAnyNumber = satisfy isNumber

matchGame :: Parser Char [Char]
matchGame = (sequenceA $ map matchChar "Game")

matchColon :: Parser Char [Char]
matchColon = (sequenceA $ map matchChar ":")

matchSemiColon :: Parser Char [Char]
matchSemiColon = (sequenceA $ map matchChar ";")

matchSpace :: Parser Char [Char]
matchSpace = (sequenceA $ map matchChar " ")

matchComma :: Parser Char [Char]
matchComma = (sequenceA $ map matchChar ",")