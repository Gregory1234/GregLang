{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving, KindSignatures, DataKinds,
  TypeOperators, FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
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
instance Show l => Treeable (ELit l t) where
  toTree (ELit l) = toTree ("lit " ++ show l)
instance (Show l, Parsable l) => IsSyntax (ELit l t)
instance (Show l, Parsable l) => IsExpr (ELit l)

newtype EVar t = EVar Ident
  deriving Parsable
instance Treeable (EVar t) where
  toTree (EVar n) = toTree ("var " ++ getIdent n)
instance IsSyntax (EVar t)
instance IsExpr EVar

newtype EParens e (n :: * -> *) t = EParens (e t)
instance Parsable (e t) => Parsable (EParens e n t) where
  parser = EParens <$> parens parser
instance Treeable (e t) => Treeable (EParens e n t) where
  toTree (EParens e) = listToTree "parens" [e]
instance IsSyntax (e t) => IsSyntax (EParens e n t)
instance (Parsable (e t), Parsable (n t)) => Parsable (ExprTFree EParens e n t) where
  parser = ExprTFree . EParens <$> try (parens parser) <|> ExprTPure <$> parser
instance IsExpr e => IsExpr (EParens e n)
instance IsExprT EParens

data EDot e n t = EDot (n t) Ident [e t]
instance (Parsable (n t), Parsable (e t)) => Parsable (EDot e n t) where
  parser = EDot <$> parser <* kw "." <*> parser <*> optionL
    (parens (maybeCommas parser))
instance (Treeable (n t), Treeable (e t)) => Treeable (EDot e n t) where
  toTree (EDot e (Ident i) a) =
    Node ("var " ++ i) (listToTree "of" [e] : toForest a)
instance (IsSyntax (n t), IsSyntax (e t)) => IsSyntax (EDot e n t)
instance (Parsable (e t), Parsable (n t)) => Parsable (ExprTFree EDot e n t) where
  parser = do
    e  <- parser
    ds <- many (preKw "." $ parser <&> optionL (parens (maybeCommas parser)))
    return (foldl ((ExprTFree .) . uncurry . EDot) (ExprTPure e) ds)
instance (IsExpr e, IsExpr n) => IsExpr (EDot e n)
instance IsExprT EDot

data EAdd (e :: * -> *) n t = EAdd (n t) (n t) | ESub (n t) (n t)
instance (Parsable (n t)) => Parsable (EAdd e n t) where
  parser = operatorParserSingle ((kw "+" $> EAdd) <|> (kw "-" $> ESub))
instance (Treeable (n t)) => Treeable (EAdd e n t) where
  toTree (EAdd a b) = listToTree "add" [a, b]
  toTree (ESub a b) = listToTree "sub" [a, b]
instance (IsSyntax (n t)) => IsSyntax (EAdd e n t)
instance Parsable (n t) => Parsable (ExprTFree EAdd e n t) where
  parser = operatorParser ((kw "+" $> EAdd) <|> (kw "-" $> ESub))
instance IsExpr n => IsExpr (EAdd e n)
instance IsExprT EAdd

data EMul (e :: * -> *) n t = EMul (n t) (n t) | EDiv (n t) (n t)
instance (Parsable (n t)) => Parsable (EMul e n t) where
  parser = operatorParserSingle ((kw "*" $> EMul) <|> (kw "/" $> EDiv))
instance (Treeable (n t)) => Treeable (EMul e n t) where
  toTree (EMul a b) = listToTree "mul" [a, b]
  toTree (EDiv a b) = listToTree "div" [a, b]
instance (IsSyntax (n t)) => IsSyntax (EMul e n t)
instance Parsable (n t) => Parsable (ExprTFree EMul e n t) where
  parser = operatorParser ((kw "*" $> EMul) <|> (kw "/" $> EDiv))
instance IsExpr n => IsExpr (EMul e n)
instance IsExprT EMul
