{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (choice, some, (<?>), try, optional, MonadParsec (eof), runParser, mkPos)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec.Debug (MonadParsecDbg(dbg))
import Control.Applicative (many, (<|>))
import Control.Monad.State (modify, runStateT)
import Parser.Types
import Parser.Lexer

parse :: Parser a -> String -> T.Text -> Either (ParseErrorBundle T.Text Void) (a, ParserState)
parse parser = runParser (runStateT parser (mkPos 1))

parseModule :: Parser Module
parseModule = Module <$> many (topLevel <* spaceConsumer) <* eof

topLevel :: Parser TopLevel
topLevel = nonIndented $ dbg "export statement" export <|> dbg "define statement" define
    where export = keyword "export" >> Export <$> assignment <?> "export statement"
          define = keyword "define" >> Define <$> assignment <?> "define statement"

expression :: Parser Expression
expression = dbg "expression" $ do
    first <- basicExpression
    rest <- many basicExpression
    return $ if null rest
        then first
        else CallExpression $ Call first rest

basicExpression :: Parser Expression
basicExpression = choice
    [ dbg "function" $ FunctionExpression <$> function
    , dbg "block" $ BlockExpression <$> block
    , dbg "let" $ LetExpression <$> parseLet
    , dbg "parenthesis" $ parens expression
    , dbg "assignment or identifier" identifierOrAssignment
    , dbg "constant" $ ConstantExpression <$> constant
    ] <?> "expression"

function :: Parser Function
function = do
    keyword "fn"
    args <- dbg "function arguments" (some identifier <?> "function arguments")
    keyword "->"
    body <- dbg "function body" (expression <?> "function body")
    return $ Function args body

call :: Parser Call
call = Call <$> dbg "callee" expression <*> dbg "arguments" (some expression) <?> "function call"

block :: Parser [Expression]
block = keyword "do" >> modify nextIndent >> indentSome expression <* modify prevIndent <?> "block expression"

constant :: Parser Constant
constant = choice
    [ BooleanConstant <$> bool
    , FloatingConstant <$> try float
    , IntegerConstant <$> integer
    ] <?> "constant"

parseLet :: Parser Assignment
parseLet = keyword "let" >> assignment <?> "let expression"

assignment :: Parser Assignment
assignment = Assignment <$> identifier <* symbol "=" <*> expression <?> "assignment"

identifierOrAssignment :: Parser Expression
identifierOrAssignment = do
    ident <- identifier
    assign <- optional $ symbol "=" >> expression
    return $ case assign of
        Just expr -> AssignmentExpression $ Assignment ident expr
        Nothing -> IdentifierExpression ident
