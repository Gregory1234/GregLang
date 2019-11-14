{-# LANGUAGE RankNTypes, QuantifiedConstraints, KindSignatures, TypeFamilies,
  TypeOperators, DataKinds, PolyKinds, GeneralizedNewtypeDeriving,
  StandaloneDeriving, UndecidableInstances, DerivingVia #-}

module GL.SyntaxTree
  ( module GL.SyntaxTree
  )
where

import           GL.Utils
import           GL.Parser
import           Text.Megaparsec                ( (<|>) )

class (Pretty t, forall a. Pretty a => Pretty (t,a), TypeParsable t)
  => IsType t where

class (Treeable e, Pretty e, Parsable e) => IsSyntax e where

class (forall e t. (IsType t, IsExprTyp e) => IsSyntax (s e t))
  => IsStatTyp s where

newtype StatTypFix s e t = StatTypFix (s (StatTypFix s e t) e t)

deriving instance (IsExprTyp e, IsStatTyp (s (StatTypFix s e t)), IsType t)
  => Treeable (StatTypFix s e t)
deriving instance (IsExprTyp e, IsStatTyp (s (StatTypFix s e t)), IsType t)
  => Pretty (StatTypFix s e t)
deriving instance (IsExprTyp e, IsStatTyp (s (StatTypFix s e t)), IsType t)
  => Parsable (StatTypFix s e t)
deriving instance (IsExprTyp e, IsStatTyp (s (StatTypFix s e t)), IsType t)
  => IsSyntax (StatTypFix s e t)
instance
  (forall t e. (IsType t, IsExprTyp e) => IsStatTyp (s (StatTypFix s e t)))
  => IsStatTyp (StatTypFix s) where

data StatTypOr s1 s2 s (e :: * -> *) t
  = STLeft (s1 s e t)
  | STRight (s2 s e t)

type family StatTypUnion a where
  StatTypUnion '[a] = a
  StatTypUnion (a:as) = StatTypOr a (StatTypUnion as)

instance (Parsable (s1 s e t), Parsable (s2 s e t))
  => Parsable (StatTypOr s1 s2 s e t) where
  parser = fmap STLeft parser <|> fmap STRight parser

instance (Pretty (s1 s e t), Pretty (s2 s e t))
  => Pretty (StatTypOr s1 s2 s e t) where
  showPP (STLeft  a) = showPP a
  showPP (STRight a) = showPP a

instance (Treeable (s1 s e t), Treeable (s2 s e t))
  => Treeable (StatTypOr s1 s2 s e t) where
  toTree (STLeft  a) = toTree a
  toTree (STRight a) = toTree a

instance (IsStatTyp (s1 s), IsStatTyp (s2 s), IsExprTyp e, IsType t)
  => IsSyntax (StatTypOr s1 s2 s e t) where

instance (IsStatTyp (s1 s), IsStatTyp (s2 s))
  => IsStatTyp (StatTypOr s1 s2 s) where


class (forall t. IsType t => IsSyntax (e t)) => IsExprTyp e where

data ExprTypFix e t = ExprTypFix t (e (ExprTypFix e t) t)

instance (IsExprTyp (e (ExprTypFix e t)), IsType t)
  => Treeable (ExprTypFix e t) where
  toTree (ExprTypFix t e) =
    let (Node a b) = toTree e in Node (showPP (t, ClearString a)) b
deriving via (PrettyTree (ExprTypFix e t))
  instance (IsExprTyp (e (ExprTypFix e t)), IsType t) => Pretty (ExprTypFix e t)
instance (IsExprTyp (e (ExprTypFix e t)), IsType t)
  => Parsable (ExprTypFix e t) where
  parser = uncurry ExprTypFix <$> parserTypeParens
instance (IsExprTyp (e (ExprTypFix e t)), IsType t) => IsSyntax (ExprTypFix e t)
instance (forall t. IsType t => IsExprTyp (e (ExprTypFix e t)))
  => IsExprTyp (ExprTypFix e)

data ExprTypOr e1 e2 e t = ETLeft (e1 e t) | ETRight (e2 e t)

type family ExprTypUnion a where
  ExprTypUnion '[a] = a
  ExprTypUnion (a:as) = ExprTypOr a (ExprTypUnion as)

instance (Parsable (e1 e t), Parsable (e2 e t))
  => Parsable (ExprTypOr e1 e2 e t) where
  parser = fmap ETLeft parser <|> fmap ETRight parser

instance (Pretty (e1 e t), Pretty (e2 e t))
  => Pretty (ExprTypOr e1 e2 e t) where
  showPP (ETLeft  a) = showPP a
  showPP (ETRight a) = showPP a

instance (Treeable (e1 e t), Treeable (e2 e t))
  => Treeable (ExprTypOr e1 e2 e t) where
  toTree (ETLeft  a) = toTree a
  toTree (ETRight a) = toTree a

instance (IsExprTyp (e1 e), IsExprTyp (e2 e), IsType t)
  => IsSyntax (ExprTypOr e1 e2 e t) where

instance (IsExprTyp (e1 e), IsExprTyp (e2 e))
  => IsExprTyp (ExprTypOr e1 e2 e) where
