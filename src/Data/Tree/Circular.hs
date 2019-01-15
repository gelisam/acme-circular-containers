module Data.Tree.Circular where

import qualified Data.Tree as Containers


data Tree a

freeze :: Containers.Tree a -> Tree a
freeze = error "not implemented yet"

thaw :: Tree a -> Containers.Tree a
thaw = error "not implemented yet"
