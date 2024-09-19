module TypeDeduction where

import qualified Data.Map.Strict as M
import Control.Monad.State (State, runState)
import TypeDeduction.Types

emptyState :: InferenceState
emptyState = InferenceState M.empty [] 0

runInference :: State InferenceState a -> (a, InferenceState)
runInference inference = runState inference emptyState
