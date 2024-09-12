{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative ( many, Alternative((<|>)) )
import Control.Monad (void)
import qualified Data.Text as T
import Text.Megaparsec (choice, some, (<?>))
import Text.Megaparsec.Char (eol)
import Types
import Lexeme

parseModule :: Parser Module
parseModule = Module <$> many topLevel

topLevel :: Parser TopLevel
topLevel = export <|> define

export :: Parser TopLevel
export = keyword "export" >> uncurry Export <$> assignment <?> "export statement"

define :: Parser TopLevel
define = keyword "define" >> uncurry Define <$> assignment <?> "define statement"

assignment :: Parser (T.Text, Expression)
assignment = (,) <$> identifier <* symbol "=" <*> expression <?> "assignment"

expression :: Parser Expression
expression = choice
    [ ConstantExpression <$> constant
    , IdentifierExpression <$> identifier
    , FunctionExpression <$> function
    , CallExpression <$> call
    , BlockExpression <$> block
    , ParenthesisExpression <$> parens expression
    ] <?> "expression"

function :: Parser Function
function = do
    void $ keyword "fn"
    args <- some identifier <?> "function arguments"
    void $ keyword "->"
    body <- expression <?> "function body"
    return $ Function args body


call :: Parser Call
call = Call <$> expression <*> some expression <?> "function call"

block :: Parser [Expression]
block = keyword "do" *> some (expression <* eol) <?> "block expression"
