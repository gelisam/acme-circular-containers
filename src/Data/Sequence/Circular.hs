module Data.Sequence.Circular where

import qualified Data.Sequence as Containers


data Seq a

freeze :: Containers.Seq a -> Seq a
freeze = error "not implemented yet"

thaw :: Seq a -> Containers.Seq a
thaw = error "not implemented yet"
