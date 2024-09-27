{-# LANGUAGE DeriveTraversable #-}

module Data.BinaryTree (BinaryTree(..)) where

data BinaryTree a
    = Leaf a
    | Branch (BinaryTree a) (BinaryTree a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Semigroup (BinaryTree a) where
    (<>) = Branch

instance Applicative BinaryTree where
    pure = Leaf
    Leaf f       <*> t            = f <$> t
    Branch f1 f2 <*> Leaf r       = Branch (($ r) <$> f1) (($ r) <$> f2)
    Branch f1 f2 <*> Branch v1 v2 = Branch (f1 <*> v1) (f2 <*> v2)

instance Monad BinaryTree where
    Leaf a     >>= f = f a
    Branch a b >>= f = Branch (a >>= f) (b >>= f)
