module TypeDeduction.Inference where

import qualified Data.Text as T
import TypeDeduction.Types
import Parser.Types
import TypeDeduction.Unification (unify, substituteAny)
import Control.Monad.State (gets)
import Data.List ((\\), group, sort)
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import TypeDeduction.Scope (value)

inferExpression :: Expression -> Inference Type
inferExpression (ConstantExpression c) = inferConstant c
inferExpression (IdentifierExpression i) = inferIdentifier i
inferExpression (FunctionExpression f) = inferFunction f
inferExpression (CallExpression c) = inferCall c
inferExpression (BlockExpression b) = inferBlock b
-- TODO: Let is not an expression
inferExpression (LetExpression l) = schemeType <$> inferLet l
inferExpression (AssignmentExpression a) = inferAssignment a

inferConstant :: Constant -> Inference Type
inferConstant (IntegerConstant _) = return $ basicType IntegerType
inferConstant (FloatingConstant _) = return $ basicType FloatingType
inferConstant (BooleanConstant _) = return $ basicType BooleanType

inferIdentifier :: T.Text -> Inference Type
inferIdentifier name = envFind name >>= instantiate

inferFunction :: Function -> Inference Type
inferFunction (Function args body) = envScope $ do
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
inferBlock l = envScope $ inferExpressions l
    where inferExpressions [expr] = inferExpression expr
          inferExpressions (expr:rest) = inferExpression expr >> inferExpressions rest
          inferExpressions [] = undefined -- blocks are never empty, and if they are than parser is failing

inferLet :: Assignment -> Inference Scheme
inferLet (Assignment variable expression) = do
    exprType <- inferExpression expression
    lastConstraint <- gets $ maybeToList . listToMaybe . inferenceConstraints -- TODO: Change API to return new constraints (there could be more than 1)
    exprScheme <- generalize lastConstraint exprType
    envInsertScheme variable exprScheme

-- TODO: Narrowing of scheme
inferAssignment :: Assignment -> Inference Type
inferAssignment (Assignment variable expression) = do
    varType <- inferIdentifier variable
    exprType <- inferExpression expression
    addConstraint $ typeConstraint varType exprType
    return varType

instantiate :: Scheme -> Inference Type
instantiate (Scheme [] t) = return t
instantiate (Scheme vars t) = do
    typeVars <- mapM ((basicType . TypeVar <$>) . const newTypeVar) vars
    let substitutions = zipWith Substitution vars typeVars
    return $ mapLeafs (substituteAny substitutions) t

generalize :: [TypeConstraint] -> Type -> Inference Scheme
generalize constraints t = do
    -- 1. Solve constraints
    let substitutions = unify constraints

    -- 2. Update environment and type
    modifyEnv $ mapEnv $ mapLeafs $ substituteAny substitutions
    let newT = mapLeafs (substituteAny substitutions) t

    -- 3. Find type variables in type and in environment
    let extractTypeVars x = case x of
            TypeVar v -> [v]
            _ -> []
    let tTypeVars = foldMap extractTypeVars newT
    envTypeVars <- gets $ foldMap (schemeVars . value) . inferenceEnvironment

    -- 4. Type variable present in type and not in environment could be generalized in type scheme
    let freeTypeVars = unique tTypeVars \\ unique envTypeVars
    return $ Scheme freeTypeVars newT

unique :: Ord a => [a] -> [a]
unique = mapMaybe listToMaybe . group . sort
