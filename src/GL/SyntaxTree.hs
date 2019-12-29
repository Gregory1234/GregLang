{-# LANGUAGE RankNTypes, QuantifiedConstraints, TypeOperators, DataKinds,
  PolyKinds, UndecidableInstances, GADTs, FlexibleInstances, EmptyCase,
  GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module GL.SyntaxTree
  ( module GL.SyntaxTree
  )
where

import           GL.Utils
import           GL.Parser
import           Text.Megaparsec                ( (<|>) )

class (Pretty t, forall a. Pretty a => Pretty (t,a), TypeParsable t) => IsType t

class (Treeable e, Pretty e, Parsable e) => IsSyntax e

data ExprTypU l t where
  ExprTypUF ::e t -> ExprTypU (e:es) t
  ExprTypUN ::ExprTypU es t -> ExprTypU (e:es) t

instance Treeable (ExprTypU '[] t) where
  toTree x = case x of {}
instance (Treeable (e t),Treeable (ExprTypU es t))
  => Treeable (ExprTypU (e:es) t) where
  toTree (ExprTypUF x ) = toTree x
  toTree (ExprTypUN xs) = toTree xs
instance Pretty (ExprTypU '[] t) where
  showPP x = case x of {}
instance (Pretty (e t),Pretty (ExprTypU es t))
  => Pretty (ExprTypU (e:es) t) where
  showPP (ExprTypUF x ) = showPP x
  showPP (ExprTypUN xs) = showPP xs
instance (Parsable (e t)) => Parsable (ExprTypU '[e] t) where
  parser = ExprTypUF <$> parser
instance (Parsable (e t), Parsable (ExprTypU (e':es) t))
  => Parsable (ExprTypU (e:e':es) t) where
  parser = (ExprTypUF <$> parser) <|> (ExprTypUN <$> parser)
instance (Treeable (ExprTypU es t), Pretty (ExprTypU es t), Parsable (ExprTypU es t))
  => IsSyntax (ExprTypU es t)

data ExprTypTDo es b c t where
  ExprTypTDoB ::b t -> ExprTypTDo '[] b c t
  ExprTypTDoF ::e c (ExprTypTDo (e:es) b c) t -> ExprTypTDo (e:es) b c t
  ExprTypTDoN ::ExprTypTDo es b c t -> ExprTypTDo (e:es) b c t

instance Treeable (b t) => Treeable (ExprTypTDo '[] b c t) where
  toTree (ExprTypTDoB x) = toTree x
instance (Treeable (e c (ExprTypTDo (e:es) b c) t), Treeable (ExprTypTDo es b c t))
  => Treeable (ExprTypTDo (e:es) b c t) where
  toTree (ExprTypTDoF x) = toTree x
  toTree (ExprTypTDoN x) = toTree x
instance Pretty (b t) => Pretty (ExprTypTDo '[] b c t) where
  showPP (ExprTypTDoB x) = showPP x
instance (Pretty (e c (ExprTypTDo (e:es) b c) t), Pretty (ExprTypTDo es b c t))
  => Pretty (ExprTypTDo (e:es) b c t) where
  showPP (ExprTypTDoF x) = showPP x
  showPP (ExprTypTDoN x) = showPP x
instance Parsable (b t) => Parsable (ExprTypTDo '[] b c t) where
  parser = ExprTypTDoB <$> parser
instance (Treeable (ExprTypTDo es b c t), Pretty (ExprTypTDo es b c t), Parsable (ExprTypTDo es b c t))
  => IsSyntax (ExprTypTDo es b c t)

newtype ExprTypFix (e :: (* -> *) -> * -> *) t
  = ExprTypFix (e (ExprTypFix e) t)

deriving instance Treeable (e (ExprTypFix e) t)
  => Treeable (ExprTypFix e t)
deriving instance Pretty (e (ExprTypFix e) t)
  => Pretty (ExprTypFix e t)
deriving instance Parsable (e (ExprTypFix e) t)
  => Parsable (ExprTypFix e t)
deriving instance IsSyntax (e (ExprTypFix e) t)
  => IsSyntax (ExprTypFix e t)

data StatTypU l e t where
  StatTypUF ::z e t -> StatTypU (z:zs) e t
  StatTypUN ::StatTypU zs e t -> StatTypU (z:zs) e t

instance Treeable (StatTypU '[] e t) where
  toTree x = case x of {}
instance (Treeable (z e t),Treeable (StatTypU zs e t))
  => Treeable (StatTypU (z:zs) e t) where
  toTree (StatTypUF x ) = toTree x
  toTree (StatTypUN xs) = toTree xs
instance Pretty (StatTypU '[] e t) where
  showPP x = case x of {}
instance (Pretty (z e t),Pretty (StatTypU zs e t))
  => Pretty (StatTypU (z:zs) e t) where
  showPP (StatTypUF x ) = showPP x
  showPP (StatTypUN xs) = showPP xs
instance (Parsable (z e t)) => Parsable (StatTypU '[z] e t) where
  parser = StatTypUF <$> parser
instance (Parsable (z e t), Parsable (StatTypU (z':zs) e t))
  => Parsable (StatTypU (z:z':zs) e t) where
  parser = (StatTypUF <$> parser) <|> (StatTypUN <$> parser)
instance (Treeable (StatTypU zs e t), Pretty (StatTypU zs e t), Parsable (StatTypU zs e t))
  => IsSyntax (StatTypU zs e t)

data StatTypTU l s e t where
  StatTypTUF ::z s e t -> StatTypTU (z:zs) s e t
  StatTypTUN ::StatTypTU zs s e t -> StatTypTU (z:zs) s e t

instance Treeable (StatTypTU '[] s e t) where
  toTree x = case x of {}
instance (Treeable (z s e t),Treeable (StatTypTU zs s e t))
  => Treeable (StatTypTU (z:zs) s e t) where
  toTree (StatTypTUF x ) = toTree x
  toTree (StatTypTUN xs) = toTree xs
instance Pretty (StatTypTU '[] s e t) where
  showPP x = case x of {}
instance (Pretty (z s e t),Pretty (StatTypTU zs s e t))
  => Pretty (StatTypTU (z:zs) s e t) where
  showPP (StatTypTUF x ) = showPP x
  showPP (StatTypTUN xs) = showPP xs
instance (Parsable (z s e t)) => Parsable (StatTypTU '[z] s e t) where
  parser = StatTypTUF <$> parser
instance (Parsable (z s e t), Parsable (StatTypTU (z':zs) s e t))
  => Parsable (StatTypTU (z:z':zs) s e t) where
  parser = (StatTypTUF <$> parser) <|> (StatTypTUN <$> parser)
instance (Treeable (StatTypTU zs s e t), Pretty (StatTypTU zs s e t), Parsable (StatTypTU zs s e t))
  => IsSyntax (StatTypTU zs s e t)

newtype StatTypFix (s :: * -> (* -> *) -> * -> *) (e :: * -> *) t
  = StatTypFix (s (StatTypFix s e t) e t)

deriving instance Treeable (s (StatTypFix s e t) e t)
  => Treeable (StatTypFix s e t)
deriving instance Pretty (s (StatTypFix s e t) e t)
  => Pretty (StatTypFix s e t)
deriving instance Parsable (s (StatTypFix s e t) e t)
  => Parsable (StatTypFix s e t)
deriving instance IsSyntax (s (StatTypFix s e t) e t)
  => IsSyntax (StatTypFix s e t)

newtype ConstStatTypT z s e t = ConstStatTypT (z e t)
  deriving (Treeable, Pretty, Parsable, IsSyntax)
