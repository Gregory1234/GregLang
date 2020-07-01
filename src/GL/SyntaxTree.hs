{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module GL.SyntaxTree
  ( module GL.SyntaxTree.Stat
  , module GL.SyntaxTree
  )
where

import           Control.Applicative
import qualified Data.Text                     as T
import           GHC.Exts

import           GL.Parser
import           GL.SyntaxTree.Stat
import           GL.Utils

newtype Package = Package [Ident]
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsList)

instance Treeable Package where
  toTree (Package path) = empTree . T.concat $ map getIdent path

instance Parsable Package where
  parser = fmap Package $ (:) <$> parser <*> many (preSm "." parser)

data Decl
  = DFun Ident [Stat]
  deriving stock (Eq, Ord, Show)

instance Treeable Decl where
  toTree (DFun n s) = listToTree ("fun " <> getIdent n) s

instance Parsable Decl where
  parser = DFun <$> parser <*> safeBraces

data AST = AST Package [Package] [Decl]
  deriving stock (Eq, Ord, Show)

instance Treeable AST where
  toTree (AST p i d) =
    Node "AST" [toTree p, listToTree "imports" i, listToTree "declarations" d]

instance Parsable AST where
  parser =
    finished
      $   AST
      <$> preKw "package" parser
      <*> many (preKw "import" parser)
      <*> many parser
