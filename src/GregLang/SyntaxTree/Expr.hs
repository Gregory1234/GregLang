{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving, KindSignatures,
  TypeOperators, DataKinds, FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
module GregLang.SyntaxTree.Expr
  ( module GregLang.SyntaxTree.Expr
  )
where

import           GL.SyntaxTree
import           GL.Parser
import           GL.Utils
import           GL.Ident
import           Text.Megaparsec

newtype ELit l t = ELit l
  deriving Parsable
  deriving Pretty via (PrettyTree (ELit l t))
instance Pretty l => Treeable (ELit l t) where
  toTree (ELit l) = toTree ("lit " ++ showPP l)
instance (Pretty l, Parsable l) => IsSyntax (ELit l t)

newtype EParens e (n :: * -> *) t = EParens (e t)
  deriving Pretty via (PrettyTree (EParens e n t))
instance Parsable (e t) => Parsable (EParens e n t) where
  parser = EParens <$> parens parser
instance Treeable (e t) => Treeable (EParens e n t) where
  toTree (EParens e) = listToTree "parens" [e]
instance IsSyntax (e t) => IsSyntax (EParens e n t)

instance (Parsable (c t), Parsable (ExprTypTDo es b c t))
  => Parsable (ExprTypTDo (EParens:es) b c t) where
  parser = (ExprTypTDoF <$> parser) <|> (ExprTypTDoN <$> parser)

data EVar e n t = EVar (Maybe (n t)) Ident [e t]
  deriving Pretty via (PrettyTree (EVar e n t))
instance (Parsable (n t), Parsable (e t)) => Parsable (EVar e n t) where
  parser = EVar <$> optional (parser <* kw ".") <*> parser <*> optionL
    (parens (maybeCommas parser))
instance (Treeable (n t), Treeable (e t)) => Treeable (EVar e n t) where
  toTree (EVar Nothing (Ident i) a) = listToTree ("var " ++ i) a
  toTree (EVar (Just e) (Ident i) a) =
    Node ("var " ++ i) (listToTree "of" [e] : toForest a)
instance (IsSyntax (n t), IsSyntax (e t)) => IsSyntax (EVar e n t)

instance (Parsable (c t), Parsable (ExprTypTDo es b c t))
  => Parsable (ExprTypTDo (EVar:es) b c t) where
  parser = do
    e  <- parser
    ds <- many (preKw "." $ parser <&> optionL (parens (maybeCommas parser)))
    return (foldl ((ExprTypTDoF .) . uncurry . EVar . Just) (ExprTypTDoN e) ds)

data EAdd (e :: * -> *) n t = EAdd (n t) (n t) | ESub (n t) (n t)
  deriving Pretty via (PrettyTree (EAdd e n t))
instance (Parsable (n t)) => Parsable (EAdd e n t) where
  parser = do
    e1 <- parser
    op <- (kw "+" $> EAdd) <|> (kw "-" $> ESub)
    op e1 <$> parser
instance (Treeable (n t)) => Treeable (EAdd e n t) where
  toTree (EAdd a b) = listToTree "add" [a, b]
  toTree (ESub a b) = listToTree "sub" [a, b]
instance (IsSyntax (n t)) => IsSyntax (EAdd e n t)

instance (Parsable (ExprTypTDo es b c t))
  => Parsable (ExprTypTDo (EAdd:es) b c t) where
  parser = do
    e  <- parser
    ds <- many
      (((kw "+" $> EAdd) <|> (kw "-" $> ESub)) <&> fmap ExprTypTDoN parser)
    return (foldl (\a (f, b) -> ExprTypTDoF (f a b)) (ExprTypTDoN e) ds)

data EMul (e :: * -> *) n t = EMul (n t) (n t) | EDiv (n t) (n t)
  deriving Pretty via (PrettyTree (EMul e n t))
instance (Parsable (n t)) => Parsable (EMul e n t) where
  parser = do
    e1 <- parser
    op <- (kw "*" $> EMul) <|> (kw "/" $> EDiv)
    op e1 <$> parser
instance (Treeable (n t)) => Treeable (EMul e n t) where
  toTree (EMul a b) = listToTree "mul" [a, b]
  toTree (EDiv a b) = listToTree "div" [a, b]
instance (IsSyntax (n t)) => IsSyntax (EMul e n t)

instance (Parsable (ExprTypTDo es b c t))
  => Parsable (ExprTypTDo (EMul:es) b c t) where
  parser = do
    e  <- parser
    ds <- many
      (((kw "*" $> EMul) <|> (kw "/" $> EDiv)) <&> fmap ExprTypTDoN parser)
    return (foldl (\a (f, b) -> ExprTypTDoF (f a b)) (ExprTypTDoN e) ds)
