{-# LANGUAGE RankNTypes, QuantifiedConstraints, KindSignatures, TypeFamilies,
  TypeOperators, DataKinds, PolyKinds #-}

module GL.SyntaxTree
  ( module GL.SyntaxTree
  )
where

import           GL.Utils
import           GL.Parser
import           Text.Megaparsec                ( (<|>) )

class (Pretty t, forall a. Pretty a => Pretty (t,a), TypeParsable t) => IsType t where

class (Treeable e, Pretty e, Parsable e) => IsSyntax e where

class (forall e t. (IsType t, IsExprTyp e) => IsSyntax (s e t)) => IsStatTyp s where

data StatTypOr s1 s2 (e :: * -> *) t = STLeft (s1 e t) | STRight (s2 e t)

type family StatTypUnion a where
  StatTypUnion '[a] = a
  StatTypUnion (a:as) = StatTypOr a (StatTypUnion as)

instance (Parsable (s1 e t), Parsable (s2 e t)) => Parsable (StatTypOr s1 s2 e t) where
  parser = fmap STLeft parser <|> fmap STRight parser

instance (Pretty (s1 e t), Pretty (s2 e t)) => Pretty (StatTypOr s1 s2 e t) where
  showPP (STLeft a) = showPP a
  showPP (STRight a) = showPP a

instance (Treeable (s1 e t), Treeable (s2 e t)) => Treeable (StatTypOr s1 s2 e t) where
  toTree (STLeft a) = toTree a
  toTree (STRight a) = toTree a

instance (IsStatTyp s1, IsStatTyp s2, IsExprTyp e, IsType t) => IsSyntax (StatTypOr s1 s2 e t) where

instance (IsStatTyp s1, IsStatTyp s2) => IsStatTyp (StatTypOr s1 s2) where

class (forall t. IsType t => IsSyntax (e t)) => IsExprTyp e where
