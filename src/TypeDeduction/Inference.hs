module TypeDeduction.Inference where

import qualified Data.Text as T
import TypeDeduction.Types
import Parser.Types

inferExpression :: Expression -> Inference Type
inferExpression (ConstantExpression c) = inferConstant c
inferExpression (IdentifierExpression i) = inferIdentifier i
inferExpression (FunctionExpression f) = inferFunction f
inferExpression (CallExpression c) = inferCall c
inferExpression (BlockExpression b) = inferBlock b
inferExpression (LetExpression l) = inferLet l
inferExpression (AssignmentExpression a) = inferAssignment a

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

inferBlock :: [Expression] -> Inference Type
inferBlock [] = undefined -- blocks are never empty, and if they are than parser is failing
inferBlock [expr] = inferExpression expr
inferBlock (expr:rest) = inferExpression expr >> inferBlock rest

-- TODO: Add TypeVariable polimorphism to functions
inferLet :: Assignment -> Inference Type
inferLet (Assignment variable expression) = do
    exprType <- inferExpression expression
    envInsert variable exprType

inferAssignment :: Assignment -> Inference Type
inferAssignment (Assignment variable expression) = do
    varType <- inferIdentifier variable
    exprType <- inferExpression expression
    addConstraint $ TypeConstraint varType exprType
    return varType
