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
inferConstant (IntegerConstant _) = return $ basicType IntegerType
inferConstant (FloatingConstant _) = return $ basicType FloatingType
inferConstant (BooleanConstant _) = return $ basicType BooleanType

inferIdentifier :: T.Text -> Inference Type
inferIdentifier name = envFind name >>= instantiate

inferFunction :: Function -> Inference Type
inferFunction (Function args body) = do
    let inferArg arg = newTypeVar >>= envInsert arg . basicType . TypeVar
    argTypes <- mapM inferArg args
    bodyType <- inferExpression body
    return $ foldr functionType bodyType argTypes

inferCall :: Call -> Inference Type
inferCall (Call callee args) = do
    calleeType <- inferExpression callee
    argTypes <- mapM inferExpression args
    resultType <- basicType . TypeVar <$> newTypeVar
    let appliedCalleeType = foldr functionType resultType argTypes
    addConstraint $ typeConstraint calleeType appliedCalleeType
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
    addConstraint $ typeConstraint varType exprType
    return varType

instantiate :: Scheme -> Inference Type
instantiate (Scheme [] t) = return t
instantiate (Scheme vars t) = do
    typeVars <- mapM (const newTypeVar) vars
    let mappings = zipWith (\x y -> (TypeVar x, TypeVar y)) vars typeVars
    let replaceTypeVars var = foldl (\x (from, to) -> if x == from then to else x) var mappings
    return $ replaceTypeVars <$> t
