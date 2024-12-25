module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> print err
        Right problem -> do
            output <- runSolution problem
            putStrLn output