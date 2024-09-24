{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DatatypeContexts #-}

module TypeDeduction.Types where

import Data.Text (Text)
import Control.Monad.State (State, MonadState (get, put), gets, modify)
import Data.Foldable (find)
import qualified Data.Kind as K

data Type
    = IntegerType
    | FloatingType
    | BooleanType
    | FunctionType {functionIn :: Type, functionOut :: Type}
    | TypeVar      TypeVariable
    deriving (Show, Eq)

type TypeVariable = Int

data Scheme = Scheme
    { schemeVars :: [TypeVariable]
    , schemeType :: Type
    } deriving (Show, Eq)

data Scope kv = Scope
    { scopeMappings :: [kv] -- Since there should not be many elements Map is not necessary
    , outerScope    :: Maybe (Scope kv)
    } deriving (Show, Functor, Foldable)

class KeyValueItem (kv :: K.Type -> K.Type -> K.Type) where
    keyValue :: k -> v -> kv k v
    key :: kv k v -> k
    value :: kv k v -> v

instance KeyValueItem (,) where
    keyValue = (,)
    key = fst
    value = snd

pushScope :: Scope kv -> Scope kv
pushScope = Scope [] . Just

popScope :: Scope kv -> Maybe (Scope kv)
popScope = outerScope

addMapping :: (KeyValueItem kv, Eq k) => k -> v -> Scope (kv k v) -> Maybe (Scope (kv k v))
addMapping k v scope@(Scope mappings outer) =
    case findMapping k scope of
        Nothing -> Just $ Scope (keyValue k v:mappings) outer
        Just _ -> Nothing

findMapping :: (KeyValueItem kv, Eq k) => k -> Scope (kv k v) -> Maybe (kv k v)
findMapping k = find ((== k) . key)

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
