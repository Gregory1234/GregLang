
module GL.SyntaxTree
  ( module GL.SyntaxTree
  , module GL.SyntaxTree.Expr
  )
where

import           GL.SyntaxTree.Expr
import           GL.Utils

data AST = AST
  deriving (Show)

instance Treeable AST where
  toTree AST = Node "AST" []
