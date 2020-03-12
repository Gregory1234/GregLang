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

data EPre (e :: * -> *) n t = ENot (n t) | ENeg (n t)
instance (Parsable (n t)) => Parsable (EPre e n t) where
  parser = ((kw "!" $> ENot) <|> (kw "-" $> ENeg)) <*> parser
instance (Treeable (n t)) => Treeable (EPre e n t) where
  toTree (ENot a) = listToTree "not" [a]
  toTree (ENeg a) = listToTree "negative" [a]
instance (IsSyntax (n t)) => IsSyntax (EPre e n t)
instance Parsable (n t) => Parsable (ExprTFree EPre e n t) where
  parser =
    ((kw "!" $> ExprTFree . ENot) <|> (kw "-" $> ExprTFree . ENeg) <|> pure id)
      <*> (ExprTPure <$> parser)
instance IsExpr n => IsExpr (EPre e n)
instance IsExprT EPre

type EAdd = EOp '[('("+", "add")), '("-", "sub")]
type EMul = EOp '[('("*", "mul")), '("/", "div"), '("%", "mod")]

type EEq = EOp '[('("==", "eq")), '("!==", "neq")]
type EComp = EOp '[('(">=", "ge")), '("<=", "le"), '(">", "gt"), '("<", "lt")]

type EAnd = EOp '[('("&", "and"))]
type EOr = EOp '[('("|", "or"))]
type EXor = EOp '[('("^", "xor"))]

type EBAnd = EOp '[('("&&", "bin and"))]
type EBOr = EOp '[('("||", "bin or"))]
type EBXor = EOp '[('("^^", "bin xor"))]
