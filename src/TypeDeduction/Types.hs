module TypeDeduction.Types where

import Data.Map.Strict
import Data.Text (Text)
import Control.Monad.State (State, MonadState (get, put), gets, modify)

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

type Environment = Map Text Scheme

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
envFind key = do
    env <- gets inferenceEnvironment
    case env !? key of
        Just val -> return val
        Nothing -> error ("Could not deduce type of " ++ show key)

envInsert :: Text -> Type -> Inference Type
envInsert key val = do
    state <- get
    let err = error ("Type deduction failed: " ++ show key ++ " already bound")
    let env = insertWith err key (Scheme [] val) $ inferenceEnvironment state
    put state {inferenceEnvironment = env}
    return val

envInsertScheme :: Text -> Scheme -> Inference Scheme
envInsertScheme key val = do
    state <- get
    let err = error ("Type deduction failed: " ++ show key ++ " already bound")
    let env = insertWith err key val $ inferenceEnvironment state
    put state {inferenceEnvironment = env}
    return val

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
