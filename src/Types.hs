module Types where

import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, Pos)
import Control.Monad.State (StateT)

type ParserState = Pos

type Parser = StateT ParserState (Parsec Void T.Text)

newtype Module = Module
    { topLevelStatements :: [TopLevel]
    } deriving (Show)

data TopLevel
    = Export Assignment
    | Define Assignment
    deriving (Show)

data Expression
    = ConstantExpression   Constant
    | IdentifierExpression T.Text
    | FunctionExpression   Function
    | CallExpression       Call
    | BlockExpression      [Expression]
    | LetExpression        Assignment
    | AssignmentExpression Assignment
    deriving (Show)

data Constant
    = IntegerConstant  Integer
    | FloatingConstant Double
    | BooleanConstant  Bool
    deriving (Show)

data Function = Function
    { functionArguments :: [T.Text]
    , functionBody      :: Expression
    } deriving (Show)

data Call = Call
    { callFunction  :: Expression
    , callArguments :: [Expression]
    } deriving (Show)

data Assignment = Assignment
    { assignmentVariable :: T.Text
    , assignmentValue    :: Expression
    } deriving (Show)
