{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Data.Tree.Circular where

import qualified Data.Tree as Containers


data Tree a = Node
  { treeParent   :: Maybe (Tree a)
  , treeLabel    :: a
  , treeChildren :: [Tree a]
  }

freeze :: forall a. Containers.Tree a -> Tree a
freeze = go Nothing
  where
    go :: Maybe (Tree a) -> Containers.Tree a -> Tree a
    go parent (Containers.Node a ts) = tree
      where
        tree :: Tree a
        tree = Node parent a children

        children :: [Tree a]
        children = go (Just tree) <$> ts

thaw :: Tree a -> Containers.Tree a
thaw = error "not implemented yet"
