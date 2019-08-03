module GL.Type where

import Data.Tree

class IsType t where
  showType :: t -> String -> String

showTypeTree :: IsType t => t -> Tree String -> Tree String
showTypeTree t (Node a b) = Node (showType t a) b

instance IsType () where
  showType () = id
