{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications, FlexibleInstances,
  UndecidableInstances, OverloadedStrings #-}
module GregLang.SyntaxTree.Type
  ( module GregLang.SyntaxTree.Type
  )
where

import           GL.Ident
import           GL.SyntaxTree
import           GL.Parser
import           GL.Utils
import           GHC.Exts
import           Text.Megaparsec               as P
import           Data.List

data Type = Type Package ClassName

instance Pretty Type where
  showPP (Type p c) = showPP p ++ '.' : classNameString c

data PartType n = FullType Type | NameType ClassName | NoType n

instance (Pretty n) => Pretty (PartType n) where
  showPP (FullType t) = showPP t
  showPP (NameType c) = showPP c
  showPP (NoType   x) = showPP x

instance (Pretty n, Pretty a) => Pretty (PartType n, a) where
  showPP (FullType t, n) = showPP n ++ " : " ++ showPP t
  showPP (NameType c, n) = showPP n ++ " : " ++ showPP c
  showPP (NoType   x, n) = showPP n ++ " : " ++ showPP x

instance TypeParsable (PartType Integer) where
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

newtype Package = Package [Ident]
  deriving IsList

instance Pretty Package where
  showPP (Package s) = intercalate "." (identString <$> s)

instance Treeable Package where
  toTree = toTree . showPP

instance Parsable Package where
  parser = Package <$> P.sepBy (parser @Ident) (kw ".")

instance IsType (PartType Integer)
