{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Wrapper.Circular (Graph(..), Vertex(..), freeze, thaw) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map, (!))
import Data.Ord (comparing)
import qualified Data.Graph.Wrapper as Wrapper
import qualified Data.Map as Map

-- $setup
-- >>> import Data.List (nub)
-- >>> import System.Mem.StableName (makeStableName)
-- >>> g = Wrapper.fromList [('1',"a",['2']), ('2',"b",['3']), ('3',"c",['1'])]
-- >>> g' = freeze g

newtype Graph i v = Graph
  { graphVertices :: Map i (Vertex i v)
  }

data Vertex i v = Vertex
  { vertexLabel      :: v
  , vertexNeighbours :: Map i (Vertex i v)
  }


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

toSortedList :: Ord i
             => Wrapper.Graph i v -> [(i, v, [i])]
toSortedList = sortBy (comparing fst3) . Wrapper.toList

-- "Data.Graph.Wrapper" doesn't have an 'Eq' instance for some reason
eqGraphWrapper :: (Ord i, Eq v)
               => Wrapper.Graph i v -> Wrapper.Graph i v -> Bool
eqGraphWrapper = (==) `on` toSortedList

-- "Data.Graph.Wrapper" doesn't have an 'Ord' instance for some reason
compareGraphWrapper :: (Ord i, Ord v)
                    => Wrapper.Graph i v -> Wrapper.Graph i v -> Ordering
compareGraphWrapper = compare `on` toSortedList

instance (Ord i, Eq v) => Eq (Graph i v) where
  (==) = eqGraphWrapper `on` thaw

instance (Ord i, Ord v) => Ord (Graph i v) where
  compare = compareGraphWrapper `on` thaw


-- |
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

-- |
-- >>> thaw (freeze g) `eqGraphWrapper` g
-- True
-- >>> freeze (thaw g') == g'
-- True
thaw :: forall i v. Ord i
     => Graph i v -> Wrapper.Graph i v
thaw = Wrapper.fromList . fmap go . Map.toList . graphVertices
  where
    go :: (i, Vertex i v) -> (i, v, [i])
    go (i, Vertex v is) = (i, v, Map.keys is)
