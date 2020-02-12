{-# LANGUAGE RankNTypes, QuantifiedConstraints, TypeOperators, DataKinds,
  PolyKinds, UndecidableInstances, GADTs, FlexibleInstances, EmptyCase,
  GeneralizedNewtypeDeriving, StandaloneDeriving, TypeFamilies #-}

module GL.SyntaxTree
  ( module GL.SyntaxTree
  )
where

import           GL.Utils
import           GL.Parser
import           Text.Megaparsec                ( (<|>) )

class (Pretty t, forall a. Pretty a => Pretty (t,a), TypeParsable t) => IsType t

class (Treeable e, Pretty e, Parsable e) => IsSyntax e

class (forall t. IsType t => IsSyntax (e t)) => IsExpr e

class (forall e n. (IsExpr e, IsExpr n) => IsExpr (f e n)) => IsExprT f

class (forall e t. (IsType t, IsExpr e) => IsSyntax (s e t)) => IsStat s

class (forall z. (IsStat z) => IsStat (s z)) => IsStatT s

data ExprEither e1 e2 t = ExprLeft (e1 t) | ExprRight (e2 t)

instance (Treeable (e1 t), Treeable (e2 t)) => Treeable (ExprEither e1 e2 t) where
  toTree (ExprLeft  x) = toTree x
  toTree (ExprRight x) = toTree x
instance (Pretty (e1 t), Pretty (e2 t)) => Pretty (ExprEither e1 e2 t) where
  showPP (ExprLeft  x) = showPP x
  showPP (ExprRight x) = showPP x
instance (Parsable (e1 t), Parsable (e2 t)) => Parsable (ExprEither e1 e2 t) where
  parser = ExprLeft <$> parser <|> ExprRight <$> parser
instance (IsSyntax (e1 t), IsSyntax (e2 t)) => IsSyntax (ExprEither e1 e2 t)
instance (IsExpr e1, IsExpr e2) => IsExpr (ExprEither e1 e2)

type family ExprUnion es where
  ExprUnion '[e] = e
  ExprUnion (e:es) = ExprEither e (ExprUnion es)

data ExprTFree f e n t = ExprTFree (f e (ExprTFree f e n) t) | ExprTPure (n t)

instance (Treeable (f e (ExprTFree f e n) t), Treeable (n t)) => Treeable (ExprTFree f e n t) where
  toTree (ExprTFree x) = toTree x
  toTree (ExprTPure x) = toTree x
instance (Pretty (f e (ExprTFree f e n) t), Pretty (n t)) => Pretty (ExprTFree f e n t) where
  showPP (ExprTFree x) = showPP x
  showPP (ExprTPure x) = showPP x
instance (Treeable (ExprTFree f e n t), Pretty (ExprTFree f e n t), Parsable (ExprTFree f e n t)) => IsSyntax (ExprTFree f e n t)
instance (forall t. IsType t => IsSyntax (ExprTFree f e n t)) => IsExpr (ExprTFree f e n)
instance (forall e n. (IsExpr e, IsExpr n) => IsExpr (ExprTFree f e n)) => IsExprT (ExprTFree f)

type family ExprTDo' fs e n where
  ExprTDo' '[f] e n = ExprTFree f e n
  ExprTDo' (f:fs) e n = ExprTFree f e (ExprTDo' fs e n)

newtype ExprTDo fs e t = ExprTDo (ExprTDo' fs (ExprTDo fs e) e t)

instance Treeable (ExprTDo' fs (ExprTDo fs e) e t) => Treeable (ExprTDo fs e t) where
  toTree (ExprTDo x) = toTree x
instance Pretty (ExprTDo' fs (ExprTDo fs e) e t) => Pretty (ExprTDo fs e t) where
  showPP (ExprTDo x) = showPP x
instance Parsable (ExprTDo' fs (ExprTDo fs e) e t) => Parsable (ExprTDo fs e t) where
  parser = ExprTDo <$> parser
instance IsSyntax (ExprTDo' fs (ExprTDo fs e) e t) => IsSyntax (ExprTDo fs e t)
instance (forall t. IsType t => IsSyntax (ExprTDo fs e t)) => IsExpr (ExprTDo fs e)

data StatTEither z1 z2 s e t = StatTLeft (z1 s e t) | StatTRight (z2 s e t)

instance (Treeable (z1 s e t), Treeable (z2 s e t)) => Treeable (StatTEither z1 z2 s e t) where
  toTree (StatTLeft  x) = toTree x
  toTree (StatTRight x) = toTree x
instance (Pretty (z1 s e t), Pretty (z2 s e t)) => Pretty (StatTEither z1 z2 s e t) where
  showPP (StatTLeft  x) = showPP x
  showPP (StatTRight x) = showPP x
instance (Parsable (z1 s e t), Parsable (z2 s e t)) => Parsable (StatTEither z1 z2 s e t) where
  parser = StatTLeft <$> parser <|> StatTRight <$> parser
instance (IsSyntax (z1 s e t), IsSyntax (z2 s e t)) => IsSyntax (StatTEither z1 z2 s e t)
instance (IsStat (z1 s), IsStat (z2 s)) => IsStat (StatTEither z1 z2 s)
instance (IsStatT z1, IsStatT z2) => IsStatT (StatTEither z1 z2)

type family StatTUnion' zs where
  StatTUnion' '[z] = z
  StatTUnion' (z:zs) = StatTEither z (StatTUnion' zs)

newtype StatTUnion zs e t = StatTUnion (StatTUnion' zs (StatTUnion zs) e t)

instance Treeable (StatTUnion' zs (StatTUnion zs) e t) => Treeable (StatTUnion zs e t) where
  toTree (StatTUnion x) = toTree x
instance Pretty (StatTUnion' zs (StatTUnion zs) e t) => Pretty (StatTUnion zs e t) where
  showPP (StatTUnion x) = showPP x
instance Parsable (StatTUnion' zs (StatTUnion zs) e t) => Parsable (StatTUnion zs e t) where
  parser = StatTUnion <$> parser
instance IsSyntax (StatTUnion' zs (StatTUnion zs) e t) => IsSyntax (StatTUnion zs e t)
instance (forall e t. (IsType t, IsExpr e) => IsSyntax (StatTUnion zs e t)) => IsStat (StatTUnion zs)
