module TypeDeduction.Types where

import Data.Set as S
import Data.Map.Strict
import Data.Text (Text)
import Control.Monad.State (State, MonadState (get, put), gets)

data Type
    = IntegerType
    | FloatingType
    | BooleanType
    | FunctionType Type Type
    | TypeVariable Int
    deriving (Show, Eq, Ord)

type Environment = Map Text Type

data TypeConstraint = TypeConstraint
    { constraintLHS :: Type
    , constraintRHS :: Type
    } deriving (Show, Eq, Ord)

data InferenceState = InferenceState
    { inferenceEnvironment  :: Environment
    , inferenceConstaints   :: Set TypeConstraint
    , inferenceTypeVarState :: Int
    } deriving (Show)

type Inference = State InferenceState

envFind :: Text -> Inference Type
envFind key = do
    env <- gets inferenceEnvironment
    case env !? key of
        Just val -> return val
        Nothing -> error ("Could not deduce type of " ++ show key)

envInsert :: Text -> Type -> Inference Type
envInsert key val = do
    state <- get
    let err = error ("Type deduction failed: " ++ show key ++ " already bound")
    let env = insertWith err key val $ inferenceEnvironment state
    put state {inferenceEnvironment = env}
    return val

addConstraint :: TypeConstraint -> Inference ()
addConstraint constraint = do
    state <- get
    let newConstraints = S.insert constraint $ inferenceConstaints state
    put state {inferenceConstaints = newConstraints}

newTypeVar :: Inference Type
newTypeVar = do
    state <- get
    let var = inferenceTypeVarState state
    put state {inferenceTypeVarState = var + 1}
    return $ TypeVariable var
