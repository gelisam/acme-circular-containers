{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Wrapper.Circular (Graph(..), Vertex(..), freeze, thaw) where

import Data.Function (on)
import Data.Map (Map, (!))
import qualified Data.Graph.Wrapper as Wrapper
import qualified Data.Map as Map

-- $setup
-- >>> import Data.List (nub)
-- >>> import System.Mem.StableName (makeStableName)
-- >>> g = Wrapper.fromList [('1',"a",['2']), ('2',"b",['3']), ('3',"c",['1'])]
-- >>> g' = freeze g

-- | A graph-based representation of a 'Wrapper.Graph': a bunch of vertices
-- (each with a unique @i@), each holding a value of type @v@ and pointers to
-- the neighbours of the vertex.
--
-- Graphs are not typically represented this way because if the graph has a
-- cycle in it, we will not be able to make any changes to the graph without
-- reallocating all the nodes which are part of that cycle and all the nodes
-- which point to it. For this reason, we do not offer any update operations.
newtype Graph i v = Graph
  { graphVertices :: Map i (Vertex i v)
  }

data Vertex i v = Vertex
  { vertexLabel      :: v
  , vertexNeighbours :: Map i (Vertex i v)
  }


instance (Ord i, Eq v) => Eq (Graph i v) where
  (==) = (==) `on` thaw


-- $
-- >>> [('1',v1), ('2',v2), ('3',v3)] = Map.toList . graphVertices $ g'
-- >>> [('2',v2')] = Map.toList . vertexNeighbours $ v1
-- >>> [('3',v3')] = Map.toList . vertexNeighbours $ v2
-- >>> [('1',v1')] = Map.toList . vertexNeighbours $ v3
-- >>> foldr seq () [v1, v1', v2, v2', v3, v3']
-- ()
-- >>> length . nub <$> mapM makeStableName [v1,v1']
-- 1
-- >>> length . nub <$> mapM makeStableName [v2,v2']
-- 1
-- >>> length . nub <$> mapM makeStableName [v3,v3']
-- 1

-- |
-- > freeze :: Ord i
-- >        -> Data.Graph.Wrapper.Graph i v
-- >        -> Data.Graph.Wrapper.Circular.Graph i v
--
-- O(n log n + k log k + k log n)
freeze :: forall i v. Ord i
       => Wrapper.Graph i v -> Graph i v
freeze g = Graph vertices
  where
    vertices :: Map i (Vertex i v)
    vertices = Map.fromList . fmap createVertex . Wrapper.toList $ g

    createVertex :: (i, v, [i]) -> (i, Vertex i v)
    createVertex (i, v, is) = (i, Vertex v neighbours)
      where
        neighbours :: Map i (Vertex i v)
        neighbours = Map.fromList . fmap lookupVertex $ is

    lookupVertex :: i -> (i, Vertex i v)
    lookupVertex i = (i, vertices ! i)

-- $
-- >>> thaw (freeze g) == g
-- True
-- >>> freeze (thaw g') == g'
-- True

-- |
-- > thaw :: Ord i
-- >      -> Data.Graph.Wrapper.Circular.Graph i v
-- >      -> Data.Graph.Wrapper.Graph i v
--
-- O(n log n + k)
thaw :: forall i v. Ord i
     => Graph i v -> Wrapper.Graph i v
thaw = Wrapper.fromList . fmap go . Map.toList . graphVertices
  where
    go :: (i, Vertex i v) -> (i, v, [i])
    go (i, Vertex v is) = (i, v, Map.keys is)
