{-# LANGUAGE RankNTypes, QuantifiedConstraints, KindSignatures, TypeFamilies,
  TypeOperators, DataKinds, PolyKinds, GeneralizedNewtypeDeriving,
  StandaloneDeriving #-}

module GL.SyntaxTree
  ( module GL.SyntaxTree
  )
where

import           GL.Utils
import           GL.Parser
import           Text.Megaparsec                ( (<|>) )

class (Pretty t, forall a. Pretty a => Pretty (t,a), TypeParsable t) => IsType t where

class (Treeable e, Pretty e, Parsable e) => IsSyntax e where

class (forall e z t. (IsType t, IsSyntax z, IsExprTyp e) => IsSyntax (s z e t)) => IsStatTyp s where

newtype StatFix s e t = StatFix (s (StatFix s e t) e t)

deriving instance (IsExprTyp e, IsStatTyp s, IsType t) => Treeable (StatFix s e t)
deriving instance (IsExprTyp e, IsStatTyp s, IsType t) => Pretty (StatFix s e t)
deriving instance (IsExprTyp e, IsStatTyp s, IsType t) => Parsable (StatFix s e t)
deriving instance (IsExprTyp e, IsStatTyp s, IsType t) => IsSyntax (StatFix s e t)

data StatTypOr s1 s2 s (e :: * -> * -> *) t = STLeft (s1 s e t) | STRight (s2 s e t)

type family StatTypUnion a where
  StatTypUnion '[a] = a
  StatTypUnion (a:as) = StatTypOr a (StatTypUnion as)

instance (Parsable (s1 s e t), Parsable (s2 s e t)) => Parsable (StatTypOr s1 s2 s e t) where
  parser = fmap STLeft parser <|> fmap STRight parser

instance (Pretty (s1 s e t), Pretty (s2 s e t)) => Pretty (StatTypOr s1 s2 s e t) where
  showPP (STLeft  a) = showPP a
  showPP (STRight a) = showPP a

instance (Treeable (s1 s e t), Treeable (s2 s e t)) => Treeable (StatTypOr s1 s2 s e t) where
  toTree (STLeft  a) = toTree a
  toTree (STRight a) = toTree a

instance (IsStatTyp s1, IsStatTyp s2, IsSyntax s, IsExprTyp e, IsType t) => IsSyntax (StatTypOr s1 s2 s e t) where

instance (IsStatTyp s1, IsStatTyp s2) => IsStatTyp (StatTypOr s1 s2) where

class (forall t f. (IsType t, IsSyntax f) => IsSyntax (e f t)) => IsExprTyp e where

newtype ExprFix e t = ExprFix (e (ExprFix e t) t)

deriving instance (IsExprTyp e, IsType t) => Treeable (ExprFix e t)
deriving instance (IsExprTyp e, IsType t) => Pretty (ExprFix e t)
deriving instance (IsExprTyp e, IsType t) => Parsable (ExprFix e t)
deriving instance (IsExprTyp e, IsType t) => IsSyntax (ExprFix e t)

data ExprTypOr e1 e2 e t = ETLeft (e1 e t) | ETRight (e2 e t)

type family ExprTypUnion a where
  ExprTypUnion '[a] = a
  ExprTypUnion (a:as) = ExprTypOr a (ExprTypUnion as)

instance (Parsable (e1 e t), Parsable (e2 e t)) => Parsable (ExprTypOr e1 e2 e t) where
  parser = fmap ETLeft parser <|> fmap ETRight parser

instance (Pretty (e1 e t), Pretty (e2 e t)) => Pretty (ExprTypOr e1 e2 e t) where
  showPP (ETLeft  a) = showPP a
  showPP (ETRight a) = showPP a

instance (Treeable (e1 e t), Treeable (e2 e t)) => Treeable (ExprTypOr e1 e2 e t) where
  toTree (ETLeft  a) = toTree a
  toTree (ETRight a) = toTree a

instance (IsExprTyp e1, IsExprTyp e2, IsSyntax e, IsType t) => IsSyntax (ExprTypOr e1 e2 e t) where

instance (IsExprTyp e1, IsExprTyp e2) => IsExprTyp (ExprTypOr e1 e2) where
