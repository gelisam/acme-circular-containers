{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Data.Tree.Circular where

import qualified Data.Tree as Containers

-- $setup
-- >>> import Data.List (nub)
-- >>> import System.Mem.StableName (makeStableName)
-- >>> t0 = Containers.Node "1" [Containers.Node "1.1" [Containers.Node "1.1.1" [], Containers.Node "1.1.2" []], Containers.Node "1.2" []]

data Tree a = Node
  { treeParent   :: Maybe (Tree a)
  , treeLabel    :: a
  , treeChildren :: [Tree a]
  }

-- |
-- >>> t1 = freeze t0
-- >>> [t11,  t12 ] = treeChildren t1
-- >>> [t111, t112] = treeChildren t11
-- >>> Just t1'   = treeParent t11
-- >>> Just t1''  = treeParent t12
-- >>> Just t11'  = treeParent t111
-- >>> Just t11'' = treeParent t112
-- >>> foldr seq () [t1, t1', t1'', t11, t11', t11'']
-- ()
-- >>> length . nub <$> mapM makeStableName [t1,t1',t1'']
-- 1
-- >>> length . nub <$> mapM makeStableName [t11,t11',t11'']
-- 1
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

-- |
-- >>> thaw (freeze t0) == t0
-- True
thaw :: Tree a -> Containers.Tree a
thaw (Node _ a ts) = Containers.Node a (fmap thaw ts)
