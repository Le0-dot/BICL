{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module TypeDeduction.Types where

import Data.Text (Text)
import Control.Monad.State (State, MonadState (get, put), gets, modify)
import TypeDeduction.Scope (Scope, findMapping, KeyValueItem (..), addMapping)
import Data.Bifunctor (Bifunctor(second))

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

-- TypeConstraint represents constraint where left value should be equal to second
type TypeConstraint = (Type, Type)

typeConstraint :: Type -> Type -> TypeConstraint
typeConstraint = (,)

data Substitution = Substitution TypeVariable Type deriving (Show)

data InferenceState = InferenceState
    { inferenceEnvironment  :: Environment
    , inferenceConstraints  :: [TypeConstraint]
    , inferenceTypeVarState :: TypeVariable
    } deriving (Show)

type Inference = State InferenceState

modifyEnv :: (Environment -> Environment) -> Inference ()
modifyEnv f = modify $ \s -> s {inferenceEnvironment = f $ inferenceEnvironment s}

mapEnv :: (Type -> Type) -> Environment -> Environment
mapEnv f = fmap $ second (\s -> s {schemeType = f (schemeType s)})

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
addConstraint constraint = modify $ \state -> state {inferenceConstraints = constraint : inferenceConstraints state}

newTypeVar :: Inference TypeVariable
newTypeVar = do
    state <- get
    let var = inferenceTypeVarState state
    put state {inferenceTypeVarState = var + 1}
    return var

mapScheme :: (BasicType -> BasicType) -> Scheme -> Scheme
mapScheme f (Scheme vars t) = Scheme vars (f <$> t)
