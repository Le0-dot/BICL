module TypeDeduction where

import Control.Monad.State (State, runState)
import TypeDeduction.Types
import TypeDeduction.Scope (emptyScope)

emptyState :: InferenceState
emptyState = InferenceState emptyScope [] 0

runInference :: State InferenceState a -> (a, InferenceState)
runInference inference = runState inference emptyState
