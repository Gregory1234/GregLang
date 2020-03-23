{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications, FlexibleInstances,
  UndecidableInstances, OverloadedStrings #-}
module GregLang.SyntaxTree.Type
  ( module GregLang.SyntaxTree.Type
  )
where

import           GL.Ident
import           GL.Type
import           GL.Parser
import           GL.Utils
import           GHC.Exts
import           Text.Megaparsec               as P
import           Data.List

data Type = Type Package ClassName

instance Treeable Type where
  toTree (Type p c) = toTree $ treePP p ++ '.' : getClassName c

data PartType n = FullType Type | NameType ClassName | NoType n

instance Treeable n => Treeable (PartType n) where
  toTree (FullType t) = toTree t
  toTree (NameType c) = toTree c
  toTree (NoType   x) = toTree x

instance IsType (PartType Integer) where
  parserType = do
    a <- optional parser
    i <- parser
    t <- maybe (NoType <$> inc) (return . NameType) a
    return (t, i)
  parserTypeParens = do
    a <- optional (parens parser)
    i <- parser
    t <- maybe (NoType <$> inc) (return . NameType) a
    return (t, i)
  parserNoType = NoType <$> inc
  typeAnnotate (FullType t) n = n ++ " : " ++ treePP t
  typeAnnotate (NameType c) n = n ++ " : " ++ treePP c
  typeAnnotate (NoType   x) n = n ++ " : " ++ show x

newtype Package = Package [Ident]
  deriving IsList

instance Treeable Package where
  toTree (Package s) = toTree $ intercalate "." (getIdent <$> s)

instance Parsable Package where
  parser = Package <$> P.sepBy (parser @Ident) (kw ".")
