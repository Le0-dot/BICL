module TypeDeduction.Types where

import Data.Map.Strict
import Data.Text (Text)
import Control.Monad.State (State, MonadState (get, put), gets, modify)

data Type
    = IntegerType
    | FloatingType
    | BooleanType
    | FunctionType {functionIn :: Type, functionOut :: Type}
    | TypeVariable Int
    deriving (Show, Eq, Ord)

type Environment = Map Text Type

data TypeConstraint = TypeConstraint
    { constraintLHS :: Type
    , constraintRHS :: Type
    } deriving (Show, Eq, Ord)

data Substitution = Substitution
    { substitutionSubject :: Type
    , substitute          :: Type
    } deriving (Show)

data InferenceState = InferenceState
    { inferenceEnvironment  :: Environment
    , inferenceConstaints   :: [TypeConstraint]
    , inferenceTypeVarState :: Int
    } deriving (Show)

type Inference = State InferenceState

isFunction :: Type -> Bool
isFunction (FunctionType _ _) = True
isFunction _ = False

isTypeVar :: Type -> Bool
isTypeVar (TypeVariable _) = True
isTypeVar _ = False

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
addConstraint constraint = modify $ \state -> state {inferenceConstaints = constraint : inferenceConstaints state}

newTypeVar :: Inference Type
newTypeVar = do
    state <- get
    let var = inferenceTypeVarState state
    put state {inferenceTypeVarState = var + 1}
    return $ TypeVariable var
