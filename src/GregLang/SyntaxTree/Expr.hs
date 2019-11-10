{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving #-}
module GregLang.SyntaxTree.Expr
  ( module GregLang.SyntaxTree.Expr
  )
where

import           GL.SyntaxTree
import           GL.Parser
import           GL.Utils
import           GregLang.SyntaxTree.Type

newtype ELit l e t = ELit l
  deriving Parsable
  deriving Pretty via (PrettyTree (ELit l e t))
instance Pretty l => Treeable (ELit l e t) where
  toTree (ELit l) = toTree ("lit " ++ showPP l)
instance (Pretty l, Parsable l) => IsSyntax (ELit l e t) where
instance (Pretty l, Parsable l) => IsExprTyp (ELit l) where
