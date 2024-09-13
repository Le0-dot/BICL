{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative ( many, Alternative((<|>)) )
import Control.Monad (void)
import qualified Data.Text as T
import Text.Megaparsec (choice, some, (<?>))
import Text.Megaparsec.Debug (MonadParsecDbg(dbg))
import Types
import Lexeme

parseModule :: Parser Module
parseModule = Module <$> many (topLevel <* spaceConsumer)

topLevel :: Parser TopLevel
topLevel = dbg "export statement" export <|> dbg "define statement" define

export :: Parser TopLevel
export = keyword "export" >> uncurry Export <$> assignment <?> "export statement"

define :: Parser TopLevel
define = keyword "define" >> uncurry Define <$> assignment <?> "define statement"

assignment :: Parser (T.Text, Expression)
assignment = (,) <$> identifier <* symbol "=" <*> expression <?> "assignment"

expression :: Parser Expression
expression = dbg "expression" (choice
    [ dbg "function" $ FunctionExpression <$> function
    , dbg "block" $ BlockExpression <$> block
    , dbg "parenthesis" $ ParenthesisExpression <$> parens expression
    , dbg "identifier" $ IdentifierExpression <$> identifier
    , dbg "constant" $ ConstantExpression <$> constant
    , dbg "call" $ CallExpression <$> call
    ] <?> "expression")

function :: Parser Function
function = dbg "function actual" $ do
    void $ keyword "fn"
    args <- dbg "function arguments" (some identifier <?> "function arguments")
    void $ keyword "->"
    body <- dbg "function body" (expression <?> "function body")
    return $ Function args body


call :: Parser Call
call = Call <$> dbg "callee" expression <*> dbg "arguments" (some expression) <?> "function call"

block :: Parser [Expression]
block = keyword "do" *> spaceConsumer *> some (expression <* spaceConsumer) <?> "block expression"
