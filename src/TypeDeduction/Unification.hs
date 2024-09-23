module TypeDeduction.Unification where

import TypeDeduction.Types (Substitution (Substitution), Type (..), TypeConstraint (TypeConstraint), mapType, mapConstraint)
import Data.Maybe (maybeToList)

unify :: [TypeConstraint] -> [Substitution]
unify [] = []
unify (constraint:rest) = maybeToList sub ++ unify newConstraints
    where (new, sub) = reduce constraint
          newConstraints = substitute sub (new ++ rest)

reduce :: TypeConstraint -> ([TypeConstraint], Maybe Substitution)
reduce (TypeConstraint left right)
    | left == right = noSubstitution []
    | areFunctions left right = noSubstitution [TypeConstraint (functionIn left) (functionIn right), TypeConstraint (functionOut left) (functionOut right)]
    | validTypeVar left right = ([], Just $ Substitution left right)
    | validTypeVar right left = ([], Just $ Substitution right left)
    | otherwise = error "Impossible to deduce types"

substitute :: Maybe Substitution -> [TypeConstraint] -> [TypeConstraint]
substitute = map . mapConstraint . mapType . maybe id substitutionFunc
    where substitutionFunc (Substitution old new) x = if x == old then new else x

areFunctions :: Type -> Type -> Bool
areFunctions (FunctionType _ _) (FunctionType _ _) = True
areFunctions _ _ = False

validTypeVar :: Type -> Type -> Bool
validTypeVar left right = isTypeVar left && (not . isTypeVar) right && left `notPartOf` right
    where isTypeVar (TypeVariable _) = True
          isTypeVar _ = False

notPartOf :: Type -> Type -> Bool
notPartOf left (FunctionType arg out) = notPartOf left arg && notPartOf left out
notPartOf left right = left /= right

noSubstitution :: [TypeConstraint] -> ([TypeConstraint], Maybe Substitution)
noSubstitution = (, Nothing)
