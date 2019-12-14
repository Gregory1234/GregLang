{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving #-}
module GregLang.SyntaxTree.Expr
  ( module GregLang.SyntaxTree.Expr
  )
where

import           GL.SyntaxTree
import           GL.Parser
import           GL.Utils
import           GL.Ident

newtype ELit l t = ELit l
  deriving Parsable
  deriving Pretty via (PrettyTree (ELit l t))
instance Pretty l => Treeable (ELit l t) where
  toTree (ELit l) = toTree ("lit " ++ showPP l)
instance (Pretty l, Parsable l) => IsSyntax (ELit l t)

newtype EParens e t = EParens (e t)
  deriving Pretty via (PrettyTree (EParens e t))
instance Parsable (e t) => Parsable (EParens e t) where
  parser = parens parser
instance Treeable (e t) => Treeable (EParens e t) where
  toTree (EParens e) = listToTree "parens" [e]
instance IsSyntax (e t) => IsSyntax (EParens e t)

data EVar e t = EVar Ident [e t]
  deriving Pretty via (PrettyTree (EVar e t))
instance Parsable (e t) => Parsable (EVar e t) where
  parser = EVar <$> parser <*> optionL (parens (maybeCommas parser))
instance Treeable (e t) => Treeable (EVar e t) where
  toTree (EVar (Ident i) a) = listToTree ("var " ++ i) a
instance IsSyntax (e t) => IsSyntax (EVar e t)
