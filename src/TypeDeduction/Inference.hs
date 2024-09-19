module TypeDeduction.Inference where

import qualified Data.Text as T
import TypeDeduction.Types
import Parser.Types

inferExpression :: Expression -> Inference Type
inferExpression (ConstantExpression c) = inferConstant c
inferExpression (IdentifierExpression i) = inferIdentifier i
inferExpression (FunctionExpression f) = inferFunction f
inferExpression (CallExpression c) = inferCall c
inferExpression (BlockExpression b) = undefined
inferExpression (LetExpression l) = undefined
inferExpression (AssignmentExpression a) = undefined

inferConstant :: Constant -> Inference Type
inferConstant (IntegerConstant _) = return IntegerType
inferConstant (FloatingConstant _) = return FloatingType
inferConstant (BooleanConstant _) = return BooleanType

inferIdentifier :: T.Text -> Inference Type
inferIdentifier = envFind

inferFunction :: Function -> Inference Type
inferFunction (Function args body) = do
    let inferArg arg = newTypeVar >>= envInsert arg
    argTypes <- mapM inferArg args
    bodyType <- inferExpression body
    return $ foldr FunctionType bodyType argTypes

inferCall :: Call -> Inference Type
inferCall (Call callee args) = do
    calleeType <- inferExpression callee
    argTypes <- mapM inferExpression args
    resultType <- newTypeVar
    let appliedCalleeType = foldr FunctionType resultType argTypes
    addConstraint $ TypeConstraint calleeType appliedCalleeType
    return resultType
