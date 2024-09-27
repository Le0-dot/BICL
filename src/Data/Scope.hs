{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Data.Scope (Scope(..), KeyValueItem(..), pushScope, popScope, addMapping, findMapping) where

import Data.Kind (Type)
import Data.Foldable (find)

data Scope kv = Scope
    { scopeMappings :: [kv] -- Since there should not be many elements Map is not necessary
    , outerScope    :: Maybe (Scope kv)
    } deriving (Show, Functor, Foldable)

instance Semigroup (Scope kv) where
    (Scope m1 o1) <> (Scope m2 o2) = Scope (m1 <> m2) (o1 <> o2)

instance Monoid (Scope kv) where
    mempty = Scope [] Nothing

class KeyValueItem (kv :: Type -> Type -> Type) where
    keyValue :: k -> v -> kv k v
    key :: kv k v -> k
    value :: kv k v -> v

instance KeyValueItem (,) where
    keyValue = (,)
    key = fst
    value = snd

pushScope :: Scope kv -> Scope kv
pushScope = Scope [] . Just

popScope :: Scope kv -> Maybe (Scope kv)
popScope = outerScope

addMapping :: (KeyValueItem kv, Eq k) => k -> v -> Scope (kv k v) -> Maybe (Scope (kv k v))
addMapping k v scope@(Scope mappings outer) =
    case findMapping k scope of
        Nothing -> Just $ Scope (keyValue k v:mappings) outer
        Just _ -> Nothing

findMapping :: (KeyValueItem kv, Eq k) => k -> Scope (kv k v) -> Maybe (kv k v)
findMapping k = find ((== k) . key)
