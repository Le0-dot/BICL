module Main where

import Data.Text.IO qualified as T
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)
import Parser (parse, parseModule)
import TypeDeduction

main :: IO ()
main = do
    [file] <- getArgs
    contents <- T.readFile file
    let out = parse parseModule file contents
    case out of
        Left err -> putStrLn $ errorBundlePretty err
        Right res -> print res
