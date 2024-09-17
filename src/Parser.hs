{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text qualified as T
import Text.Megaparsec (choice, some, (<?>))
import Text.Megaparsec.Debug (MonadParsecDbg(dbg))
import Control.Applicative (many, (<|>))
import Control.Monad.State (modify)
import Types
import Lexeme

parseModule :: Parser Module
parseModule = Module <$> many (topLevel <* spaceConsumer)

topLevel :: Parser TopLevel
topLevel = nonIndented $ dbg "export statement" export <|> dbg "define statement" define
    where export = keyword "export" >> uncurry Export <$> assignment <?> "export statement"
          define = keyword "define" >> uncurry Define <$> assignment <?> "define statement"

assignment :: Parser (T.Text, Expression)
assignment = (,) <$> identifier <* symbol "=" <*> expression <?> "assignment"

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
    , dbg "let" $ LetExpression <$> letExpr
    , dbg "parenthesis" $ parens expression
    , dbg "identifier" $ IdentifierExpression <$> identifier
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
block = keyword "do" >> modify nextIndent >> indentSome expression <* modify prevIndent

constant :: Parser Constant
constant = choice
    [ IntegerConstant <$> integer
    , BooleanConstant <$> bool
    ]

letExpr :: Parser Let
letExpr = keyword "let" >> uncurry Let <$> assignment
