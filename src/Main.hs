module Main where

import Data.Text.IO qualified as T
import Lexeme
import Parser
import System.Environment (getArgs)
import Text.Megaparsec (MonadParsec (eof), between, errorBundlePretty, parse)

main :: IO ()
main = do
    [file] <- getArgs
    contents <- T.readFile file
    let out = parse (between spaceConsumer eof parseModule) file contents
    case out of
        Left err -> putStrLn $ errorBundlePretty err
        Right res -> print res
