{-# LANGUAGE ScopedTypeVariables #-}
module Data.Tree.Circular where

import Data.Tree (Tree(Node))

-- $setup
-- >>> import Data.List (nub)
-- >>> import System.Mem.StableName (makeStableName)
-- >>> t0 = Node "1" [Node "1.1" [Node "1.1.1" [], Node "1.1.2" []], Node "1.2" []]

data TreeNode a = TreeNode
  { treeNodeParent   :: Maybe (TreeNode a)
  , treeNodeLabel    :: a
  , treeNodeChildren :: [TreeNode a]
  }

-- |
-- >>> t1 = freeze t0
-- >>> [t11,  t12 ] = treeNodeChildren t1
-- >>> [t111, t112] = treeNodeChildren t11
-- >>> Just t1'   = treeNodeParent t11
-- >>> Just t1''  = treeNodeParent t12
-- >>> Just t11'  = treeNodeParent t111
-- >>> Just t11'' = treeNodeParent t112
-- >>> foldr seq () [t1, t1', t1'', t11, t11', t11'']
-- ()
-- >>> length . nub <$> mapM makeStableName [t1,t1',t1'']
-- 1
-- >>> length . nub <$> mapM makeStableName [t11,t11',t11'']
-- 1
freeze :: forall a. Tree a -> TreeNode a
freeze = go Nothing
  where
    go :: Maybe (TreeNode a) -> Tree a -> TreeNode a
    go parent (Node a ts) = treeNode
      where
        treeNode :: TreeNode a
        treeNode = TreeNode parent a children

        children :: [TreeNode a]
        children = go (Just treeNode) <$> ts

-- |
-- >>> thaw (freeze t0) == t0
-- True
thaw :: TreeNode a -> Tree a
thaw (TreeNode _ a ts) = Node a (fmap thaw ts)
