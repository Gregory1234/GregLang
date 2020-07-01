{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module GL.SyntaxTree.Stat
  ( module GL.SyntaxTree.Expr
  , module GL.SyntaxTree.Stat
  )
where

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
  | SBraces [Stat]
  | SExpr Expr
  | SNoOp
  deriving stock (Eq, Ord, Show)

instance Treeable Stat where
  toTree (SSet i op e) =
    listToTree (getIdent i <> " " <> maybe "" fromOperator op <> "=") [e]
  toTree (SLet i e       ) = listToTree ("let " <> getIdent i <> " =") [e]
  toTree (SIf e s Nothing) = Node "if" [toTree e, listToTree "then" [s]]
  toTree (SIf e s (Just s2)) =
    Node "if" [toTree e, listToTree "then" [s], listToTree "else" [s2]]
  toTree (SWhile   e s) = Node "while" [toTree e, listToTree "do" [s]]
  toTree (SDoWhile e s) = Node "do" [toTree s, listToTree "while" [e]]
  toTree (SBraces s   ) = listToTree "braces" s
  toTree (SExpr   e   ) = toTree e
  toTree SNoOp          = empTree "noop"

sc :: Parser a -> Parser a
sc = (<* optional (sm ";"))

instance Parsable Stat where
  parser = asum
    [ sc $ SLet <$> preKw "let" parser <*> preSm "=" parser
    , SIf <$> preKw "if" parser <*> parser <*> optional (preKw "else" parser)
    , SWhile <$> preKw "while" parser <*> parser
    , sc $ SDoWhile <$> preKw "do" parser <*> preKw "while" parser
    , SBraces <$> safeBraces
    , sc $ try (SSet <$> parser <*> parser) <*> parser
    , sc $ SExpr <$> parser
    , sm ";" $> SNoOp
    ]
