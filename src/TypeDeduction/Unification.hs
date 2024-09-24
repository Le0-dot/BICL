module TypeDeduction.Unification where

import TypeDeduction.Types (Substitution (Substitution), Type, TypeConstraint, BinaryTree (Leaf, BinaryNode), BasicType (..), typeConstraint, basicType, mapLeafs)
import Data.Maybe (maybeToList)
import Data.Monoid (All (All, getAll))
import Data.Bifunctor (Bifunctor(bimap))

unify :: [TypeConstraint] -> [Substitution]
unify [] = []
unify (constraint:rest) = maybeToList sub ++ unify newConstraints
    where (new, sub) = reduce constraint
          newConstraints = substitute sub (new ++ rest)

reduce :: TypeConstraint -> ([TypeConstraint], Maybe Substitution)
reduce (left, right)
    | left == right = ([], Nothing)
reduce (BinaryNode leftIn leftOut, BinaryNode rightIn rightOut)
    = ([typeConstraint leftIn rightIn, typeConstraint leftOut rightOut], Nothing)
reduce (left@(Leaf (TypeVar var)), right)
    | left `notPartOf` right = ([], Just $ Substitution var right)
reduce (left, right@(Leaf (TypeVar var)))
    | right `notPartOf` left = ([], Just $ Substitution var left)
reduce _ = error "Impossible to deduce types"

substitute :: Maybe Substitution -> [TypeConstraint] -> [TypeConstraint]
substitute Nothing = id
substitute (Just (Substitution old new)) = fmap $ bimap (mapLeafs sub) (mapLeafs sub)
    -- in each type constraint for each of 2 types replace all type variable that match substitution
    where sub x = if x == basicType (TypeVar old) then new else x

notPartOf :: Type -> Type -> Bool
notPartOf left = getAll . foldMap (All . (/= left) . basicType)
