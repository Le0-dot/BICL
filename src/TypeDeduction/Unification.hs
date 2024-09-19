module TypeDeduction.Unification where

import TypeDeduction.Types (Substitution, Type (..), TypeConstraint (TypeConstraint), isFunction, isTypeVar)
import Data.Maybe (maybeToList)

unify :: [TypeConstraint] -> [Substitution]
unify [] = []
unify (constraint:rest) = maybeToList sub ++ unify (newConstraints ++ rest)
    where (newConstraints, sub) = reduce constraint

reduce :: TypeConstraint -> ([TypeConstraint], Maybe Substitution)
reduce (TypeConstraint left right)
    | left == right = noSubstitution []
    | isFunction left && isFunction right = noSubstitution [TypeConstraint (functionIn left) (functionIn right), TypeConstraint (functionOut left) (functionOut right)]
    | isTypeVar left && not (isTypeVar left) = undefined -- TODO: Check if left is not part of right and implement reduce
    | not (isTypeVar left) && isTypeVar right = reduce $ TypeConstraint right left
    | otherwise = undefined

noSubstitution :: [TypeConstraint] -> ([TypeConstraint], Maybe Substitution)
noSubstitution = (, Nothing)
