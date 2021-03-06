{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Data.Sequence.Circular(Seq, NonEmptySeq(..), SeqNode(..), freeze, thaw) where

import qualified Data.Sequence as Containers

-- $setup
-- >>> import Data.List (nub)
-- >>> import System.Mem.StableName (makeStableName)
-- >>> s = Containers.fromList "abcd"

-- | A variant of 'Containers.Seq' in which we can walk from the outside in, as
-- usual, but also from the inside out.
type Seq a = Maybe (NonEmptySeq a)

-- | A doubly-linked list.
--
-- Sequences are not typically represented this way because the cycles between
-- each node and its neighbours mean that any change to the sequence requires
-- reallocating all of its node, not just the path from the closest end to the
-- modified element. For this reason, we do not offer any update operations.
data NonEmptySeq a = NonEmptySeq
  { seqFirst :: SeqNode a
  , seqLast  :: SeqNode a
  }

data SeqNode a = SeqNode
  { seqNodePrev  :: Maybe (SeqNode a)
  , seqNodeLabel :: a
  , seqNodeNext  :: Maybe (SeqNode a)
  }

-- $
-- >>> Just (NonEmptySeq s1 s4') = freeze s
-- >>> SeqNode Nothing 'a' (Just s2) = s1
-- >>> SeqNode (Just s1') 'b' (Just s3) = s2
-- >>> SeqNode (Just s2') 'c' (Just s4) = s3
-- >>> SeqNode (Just s3') 'd' Nothing = s4
-- >>> foldr seq () [s1, s1', s2, s2', s3, s3', s4, s4']
-- ()
-- >>> length . nub <$> mapM makeStableName [s1,s1']
-- 1
-- >>> length . nub <$> mapM makeStableName [s2,s2']
-- 1
-- >>> length . nub <$> mapM makeStableName [s3,s3']
-- 1
-- >>> length . nub <$> mapM makeStableName [s4,s4']
-- 1

-- |
-- > freeze :: Data.Sequence.Seq a
-- >        -> Data.Sequence.Circular.Seq a
--
-- O(n)
freeze :: forall a. Containers.Seq a -> Seq a
freeze = freezeSeq Nothing
  where
    freezeSeq :: Maybe (SeqNode a) -> Containers.Seq a -> Seq a
    freezeSeq prev = \case
      Containers.Empty    -> Nothing
      a Containers.:<| as -> Just $ freezeNonEmptySeq prev a as

    freezeNonEmptySeq :: Maybe (SeqNode a) -> a -> Containers.Seq a -> NonEmptySeq a
    freezeNonEmptySeq prev a as = NonEmptySeq first last_
      where
        first :: SeqNode a
        first = SeqNode prev a next

        seq_ :: Seq a
        seq_ = freezeSeq (Just first) as

        next :: Maybe (SeqNode a)
        next = fmap seqFirst seq_

        last_ :: SeqNode a
        last_ = maybe first seqLast seq_

-- $
-- >>> thaw (freeze s) == s
-- True

-- | Returns the elements at and to the right of the given 'SeqNode'.
--
-- > thaw :: Data.Sequence.Circular.Seq a
-- >      -> Data.Sequence.Seq a
--
-- O(n)
thaw :: forall a. Seq a -> Containers.Seq a
thaw = go . fmap seqFirst
  where
    go :: Maybe (SeqNode a) -> Containers.Seq a
    go Nothing                   = Containers.Empty
    go (Just (SeqNode _ a next)) = a Containers.:<| go next
