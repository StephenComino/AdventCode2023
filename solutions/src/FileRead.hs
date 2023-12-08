module FileRead
    ( readSampleFile
    ) where

import System.Directory

readSampleFile :: FilePath -> IO [String]
readSampleFile path = do
    exedir <- getCurrentDirectory
    putStrLn exedir
    fmap lines $ readFile (exedir ++ path)