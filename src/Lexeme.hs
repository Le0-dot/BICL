{-# LANGUAGE OverloadedStrings #-}

module Lexeme where

import Text.Megaparsec.Char (hspace1, alphaNumChar, char, letterChar, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Types
import qualified Data.Text as T
import Text.Megaparsec (between, MonadParsec (notFollowedBy, takeWhileP), chunk, (<?>), choice)
import Control.Applicative (Alternative((<|>)))
import Control.Monad (void)
import Data.Char (isAlphaNum)

(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<||>) = liftA2 (||)

hspaceConsumer :: Parser ()
hspaceConsumer = L.space
    hspace1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

spaceConsumer :: Parser ()
spaceConsumer = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol hspaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

keyword :: T.Text -> Parser ()
keyword word = lexeme $ do
    void $ chunk word
    notFollowedBy $ alphaNumChar <|> alphaNumChar <|> char '_'

identifier :: Parser T.Text
identifier = lexeme $ do
    first <- letterChar <|> char '_'
    rest <- takeWhileP (Just "alpha-numeric, \"_\"") $ isAlphaNum <||> (== '_')
    return $ first `T.cons` rest

constant :: Parser Constant
constant = lexeme $ choice
    [ IntegerConstant <$> integer
    , BooleanConstant <$> bool
    ]

bool :: Parser Bool
bool = (False <$ keyword "false" <|> True <$ keyword "true") <?> "boolean constant"

integer :: Parser Integer
integer = binary <|> hexadecimal <|> decimal

binary :: Parser Integer
binary = chunk "0b" >> L.binary <?> "binary integer constant"

decimal :: Parser Integer
decimal = L.decimal <?> "decimal integer constant"

hexadecimal :: Parser Integer
hexadecimal = chunk "0x" >> L.hexadecimal <?> "hexadecimal integer constant"
