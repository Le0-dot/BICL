module Main where

import Data.Text.IO qualified as T
import Data.Text qualified as T
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec (MonadParsec (eof), between, errorBundlePretty, runParser, ParseErrorBundle)
import Text.Megaparsec.Pos (mkPos)
import Control.Monad.State (runStateT)
import Types
import Lexeme
import Parser

parse :: Parser a -> String -> T.Text -> Either (ParseErrorBundle T.Text Void) (a, ParserState)
parse parser = runParser (runStateT parser (mkPos 1))

main :: IO ()
main = do
    [file] <- getArgs
    contents <- T.readFile file
    let parser = between spaceConsumer (spaceConsumer >> eof) parseModule
    let out = parse parser file contents
    case out of
        Left err -> putStrLn $ errorBundlePretty err
        Right res -> print res
