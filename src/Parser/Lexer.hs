{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer where

import Data.Text qualified as T
import Data.Char (isAlphaNum)
import Text.Megaparsec.Char (hspace1, alphaNumChar, char, letterChar, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec (between, MonadParsec (notFollowedBy, takeWhileP, try), chunk, (<?>), mkPos, unPos)
import Text.Megaparsec.Pos (Pos)
import Control.Applicative ( Alternative((<|>)), some, empty )
import Control.Monad (void)
import Control.Monad.State (get)
import Parser.Types

(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<||>) = liftA2 (||)

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "/*" "*/"

hspaceConsumer :: Parser ()
hspaceConsumer = L.space
    hspace1
    lineComment
    blockComment

spaceConsumer :: Parser ()
spaceConsumer = L.space
    space1
    lineComment
    blockComment

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented spaceConsumer

indentSome :: Parser a -> Parser [a]
indentSome p = do
    indent <- get
    some $ try $ L.indentGuard spaceConsumer EQ indent >> p

nextIndent :: Pos -> Pos
nextIndent = (<>) $ mkPos 4

prevIndent :: Pos -> Pos
prevIndent = mkPos . (+ (-4)) . unPos

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol hspaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

keyword :: T.Text -> Parser ()
keyword word = lexeme $ do
    void $ chunk word
    notFollowedBy $ alphaNumChar <|> char '_'

identifier :: Parser T.Text
identifier = lexeme $ do
    first <- letterChar <|> char '_'
    rest <- takeWhileP (Just "alpha-numeric, \"_\"") $ isAlphaNum <||> (== '_')
    return $ first `T.cons` rest

bool :: Parser Bool
bool = lexeme (False <$ keyword "false" <|> True <$ keyword "true" <?> "boolean constant")

integer :: Parser Integer
integer = lexeme $ signed $ bin <|> oct <|> hex <|> dec
    where bin = chunk "0b" >> L.binary <?> "binary integer constant"
          oct = chunk "0o" >> L.octal <?> "octal integer constant"
          hex = chunk "0x" >> L.hexadecimal <?> "hexadecimal integer constant"
          dec = L.decimal <?> "decimal integer constant"

float :: Parser Double
float = lexeme $ signed L.float

signed :: Num a => Parser a -> Parser a
signed = L.signed empty
