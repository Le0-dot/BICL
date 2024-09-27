module TypeDeduction where

import Control.Monad.State (State, runState)
import TypeDeduction.Types

runInference :: State InferenceState a -> (a, InferenceState)
runInference inference = runState inference $ InferenceState mempty [] 0
