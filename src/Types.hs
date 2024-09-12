module Types where

import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void T.Text

newtype Module = Module
    { topLevelStatements :: [TopLevel]
    } deriving (Show)

data TopLevel
    = Export T.Text Expression
    | Define T.Text Expression
    deriving (Show)

data Expression
    = ConstantExpression    Constant
    | IdentifierExpression  T.Text
    | FunctionExpression    Function
    | CallExpression        Call
    | BlockExpression       [Expression]
    | ParenthesisExpression Expression
    deriving (Show)

data Constant
    = IntegerConstant Integer
    | BooleanConstant Bool
    deriving (Show)

data Function = Function
    { functionArguments :: [T.Text]
    , functionBody      :: Expression
    } deriving (Show)

data Call = Call
    { callFunction  :: Expression
    , callArguments :: [Expression]
    } deriving (Show)
