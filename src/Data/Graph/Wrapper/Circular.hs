module Data.Graph.Wrapper.Circular where

import qualified Data.Graph.Wrapper as Wrapper


data Graph i v

freeze :: Wrapper.Graph i v -> Graph i v
freeze = error "not implemented yet"

thaw :: Graph i v -> Wrapper.Graph i v
thaw = error "not implemented yet"
