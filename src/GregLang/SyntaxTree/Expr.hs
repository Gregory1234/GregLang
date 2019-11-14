{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving #-}
module GregLang.SyntaxTree.Expr
  ( module GregLang.SyntaxTree.Expr
  )
where

import           GL.SyntaxTree
import           GL.Parser
import           GL.Utils
import           GL.Ident

newtype ELit l e t = ELit l
  deriving Parsable
  deriving Pretty via (PrettyTree (ELit l e t))
instance Pretty l => Treeable (ELit l e t) where
  toTree (ELit l) = toTree ("lit " ++ showPP l)
instance (Pretty l, Parsable l) => IsSyntax (ELit l e t)
instance (Pretty l, Parsable l) => IsExprTyp (ELit l e)

newtype EParens e t = EParens e
  deriving Pretty via (PrettyTree (EParens e t))
instance Parsable e => Parsable (EParens e t) where
  parser = parens parser
instance Treeable e => Treeable (EParens e t) where
  toTree (EParens e) = listToTree "parens" [e]
instance IsSyntax e => IsSyntax (EParens e t)
instance IsSyntax e => IsExprTyp (EParens e)

data EVar e t = EVar Ident [e]
  deriving Pretty via (PrettyTree (EVar e t))
instance Parsable e => Parsable (EVar e t) where
  parser = EVar <$> parser <*> optionL (parens (maybeCommas parser))
instance Treeable e => Treeable (EVar e t) where
  toTree (EVar (Ident i) a) = listToTree ("var " ++ i) a
instance IsSyntax e => IsSyntax (EVar e t)
instance IsSyntax e => IsExprTyp (EVar e)
