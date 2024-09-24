{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module TypeDeduction.Types where

import Data.Text (Text)
import Control.Monad.State (State, MonadState (get, put), gets, modify)
import TypeDeduction.Scope (Scope, findMapping, KeyValueItem (..), addMapping)

data BinaryTree a
    = Leaf a
    | BinaryNode (BinaryTree a) (BinaryTree a)
    deriving (Show, Eq, Functor, Foldable)

mapLeafs :: (BinaryTree a -> BinaryTree b) -> BinaryTree a -> BinaryTree b
mapLeafs f node@(Leaf _) = f node
mapLeafs f (BinaryNode l r) = BinaryNode (mapLeafs f l) (mapLeafs f r)

data BasicType
    = IntegerType
    | FloatingType
    | BooleanType
    | TypeVar TypeVariable
    deriving (Show, Eq)

type Type = BinaryTree BasicType

basicType :: BasicType -> Type
basicType = Leaf

functionType :: Type -> Type -> Type
functionType = BinaryNode

type TypeVariable = Int

data Scheme = Scheme
    { schemeVars :: [TypeVariable]
    , schemeType :: Type
    } deriving (Show, Eq)

type Environment = Scope (Text, Scheme)

data TypeConstraint = TypeConstraint
    { constraintLHS :: Type
    , constraintRHS :: Type
    } deriving (Show, Eq)

data Substitution = Substitution Type Type deriving (Show)

data InferenceState = InferenceState
    { inferenceEnvironment  :: Environment
    , inferenceConstaints   :: [TypeConstraint]
    , inferenceTypeVarState :: TypeVariable
    } deriving (Show)

type Inference = State InferenceState

envFind :: Text -> Inference Scheme
envFind k = do
    env <- gets inferenceEnvironment
    case findMapping k env of
        Just kv -> return $ value kv
        Nothing -> error ("Could not deduce type of " ++ show k)

envInsert :: Text -> Type -> Inference Type
envInsert k v = do
    state <- get
    let env = addMapping k (Scheme [] v) $ inferenceEnvironment state
    case env of
        Nothing -> error ("Could not infer " ++ show k ++ ", name was already bound")
        Just e -> put state {inferenceEnvironment = e} >> return v

envInsertScheme :: Text -> Scheme -> Inference Scheme
envInsertScheme k v = do
    state <- get
    let env = addMapping k v $ inferenceEnvironment state
    case env of
        Nothing -> error ("Could not infer " ++ show k ++ ", name was already bound")
        Just e -> put state {inferenceEnvironment = e} >> return v

addConstraint :: TypeConstraint -> Inference ()
addConstraint constraint = modify $ \state -> state {inferenceConstaints = constraint : inferenceConstaints state}

newTypeVar :: Inference TypeVariable
newTypeVar = do
    state <- get
    let var = inferenceTypeVarState state
    put state {inferenceTypeVarState = var + 1}
    return var

mapType :: (Type -> Type) -> Type -> Type
mapType f (FunctionType arg out) = FunctionType (mapType f arg) (mapType f out)
mapType f t = f t

mapScheme :: (Type -> Type) -> Scheme -> Scheme
mapScheme f (Scheme vars t) = Scheme vars (mapType f t)

mapConstraint :: (Type -> Type) -> TypeConstraint -> TypeConstraint
mapConstraint f (TypeConstraint left right) = TypeConstraint (f left) (f right)
