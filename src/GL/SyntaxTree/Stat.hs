{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module GL.SyntaxTree.Stat
  ( module GL.SyntaxTree.Expr
  , module GL.SyntaxTree.Stat
  )
where

import           Control.Applicative

import           GL.Token.Keyword
import           GL.Parser
import           GL.SyntaxTree.Expr
import           GL.Utils

data Stat
  = SSet Ident (Maybe Operator) Expr
  | SLet Ident Expr
  | SIf Expr Stat (Maybe Stat)
  | SWhile Expr Stat
  | SDoWhile Stat Expr
  | SExpr Expr
  deriving stock (Eq, Ord, Show)

instance Treeable Stat where
  toTree (SSet i op e) =
    listToTree (getIdent i <> " " <> maybe "" fromOperator op <> "=") [e]
  toTree (SLet i e       ) = listToTree ("let " <> getIdent i <> " =") [e]
  toTree (SIf e s Nothing) = Node "if" [toTree e, listToTree "then" [s]]
  toTree (SIf e s (Just s2)) =
    Node "if" [toTree e, listToTree "then" [toTree s, listToTree "else" [s2]]]
  toTree (SWhile   e s) = Node "while" [toTree e, listToTree "do" [s]]
  toTree (SDoWhile e s) = Node "do" [toTree s, listToTree "while" [e]]
  toTree (SExpr e     ) = toTree e

instance Parsable Stat where
  parser = asum
    [ SLet <$> preKw "let" parser <*> preSm "=" parser
    , SIf <$> preKw "if" (parens parser) <*> parser <*> optional parser
    , SWhile <$> preKw "while" (parens parser) <*> parser
    , SDoWhile <$> preKw "do" parser <*> preKw "while" (parens parser)
    , try (SSet <$> parser <*> parser) <*> parser
    , SExpr <$> parser
    ]
