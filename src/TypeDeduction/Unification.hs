module TypeDeduction.Unification where

import TypeDeduction.Types (Substitution (Substitution), Type, TypeConstraint, BasicType (..), typeConstraint)
import Data.Maybe (maybeToList)
import Data.Monoid (All (All, getAll))
import Data.Bifunctor (Bifunctor(bimap))
import Data.BinaryTree (BinaryTree(..))

unify :: [TypeConstraint] -> [Substitution]
unify [] = []
unify (constraint:rest) = maybeToList sub ++ unify (substituteConstraints (new ++ rest))
    where (new, sub) = reduce constraint
          substituteConstraints cs = case sub of
            Just s -> both (substitute s) <$> cs
            Nothing -> cs

reduce :: TypeConstraint -> ([TypeConstraint], Maybe Substitution)
reduce (left, right)
    | left == right = ([], Nothing)
reduce (Branch leftIn leftOut, Branch rightIn rightOut)
    = ([typeConstraint leftIn rightIn, typeConstraint leftOut rightOut], Nothing)
reduce (left@(Leaf (TypeVar var)), right)
    | left `notPartOf` right = ([], Just $ Substitution var right)
reduce (left, right@(Leaf (TypeVar var)))
    | right `notPartOf` left = ([], Just $ Substitution var left)
reduce _ = error "Impossible to deduce types"

substitute :: Substitution -> Type -> Type
substitute (Substitution from to) = (=<<) (\x -> if x == TypeVar from then to else return x)

substituteAny :: [Substitution] -> Type -> Type
substituteAny = flip $ foldl (flip substitute)

notPartOf :: Type -> Type -> Bool
notPartOf left = getAll . foldMap (All . (/= left) . return)

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f
