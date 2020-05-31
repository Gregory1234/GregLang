
module GL.SyntaxTree
  ( module GL.SyntaxTree
  )
where

import           GL.Utils

data AST = AST
  deriving (Show)

instance Treeable AST where
  toTree AST = Node "AST" []
