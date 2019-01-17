{-# LANGUAGE ScopedTypeVariables #-}
module Data.Tree.Circular (TreeNode(..), freeze, thaw) where

import Data.Tree (Tree(Node))

-- $setup
-- >>> import Data.List (nub)
-- >>> import System.Mem.StableName (makeStableName)
-- >>> t0 = Node "1" [Node "1.1" [Node "1.1.1" [], Node "1.1.2" []], Node "1.2" []]

-- | A variant of 'Tree' in which we can walk from the root towards the leaves,
-- as usual, but also from a leaf towards the root.
--
-- Trees are not typically represented this way because the cycles between each
-- node and its parent mean that any change to the tree requires reallocating
-- all of its node, not just the path from the root to the modified element. For
-- this reason, we do not offer any update operations.
data TreeNode a = TreeNode
  { treeNodeParent   :: Maybe (TreeNode a)
  , treeNodeLabel    :: a
  , treeNodeChildren :: [TreeNode a]
  }

-- $
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

-- | Returns the root 'TreeNode'. O(n)
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

-- $
-- >>> thaw (freeze t0) == t0
-- True

-- | Returns the 'Tree' rooted at the given 'TreeNode'. O(n)
thaw :: TreeNode a -> Tree a
thaw (TreeNode _ a ts) = Node a (fmap thaw ts)
