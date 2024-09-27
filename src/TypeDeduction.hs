module TypeDeduction where

import Control.Monad.State (State, runState)
import TypeDeduction.Types
import Data.Scope (newScope)

runInference :: State InferenceState a -> (a, InferenceState)
runInference inference = runState inference $ InferenceState newScope [] 0
