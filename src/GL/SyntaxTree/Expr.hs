{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module GL.SyntaxTree.Expr
  ( module GL.SyntaxTree.Expr
  )
where

import           Data.Char
import qualified Data.Text                     as T

import           GL.Parser
import           GL.Token.Keyword
import           GL.Utils

data Expr
  = EVar Ident (Maybe [Expr])
  | EStr Text
  | EInt Integer
  | EReal Double
  | EChar Char
  | EBool Bool
  | EThis
  | EParens Expr
  | EBinOp Expr Operator Expr
  | EBinCm Expr Comparasion Expr
  | ENot Expr
  | EBNot Expr
  | EIf Expr Expr Expr
  | EOf Expr Ident (Maybe [Expr])
  deriving stock (Eq, Ord, Show)

instance Treeable Expr where
  toTree EThis            = empTree "this"
  toTree (EVar i Nothing) = toTree $ "ident " <> getIdent i
  toTree (EVar i (Just es)) =
    listToTree (getIdent i <> "()") [listToTree "args" es]
  toTree (EStr    s     ) = toTree $ "string " <> showT s
  toTree (EInt    i     ) = toTree $ "int " <> showT i
  toTree (EReal   r     ) = toTree $ "real " <> showT r
  toTree (EChar   c     ) = toTree $ "char " <> showT c
  toTree (EBool   b     ) = toTree $ "bool " <> T.map toLower (showT b)
  toTree (EParens e     ) = listToTree "parens" [e]
  toTree (EBinOp e1 o e2) = listToTree (T.map toLower (showT o)) [e1, e2]
  toTree (EBinCm e1 o e2) = listToTree (T.map toLower (showT o)) [e1, e2]
  toTree (ENot  e       ) = toTree $ listToTree "not" [e]
  toTree (EBNot e       ) = toTree $ listToTree "bnot" [e]
  toTree (EIf e1 e2 e3) =
    Node "if" [toTree e1, listToTree "then" [e2], listToTree "else" [e3]]
  toTree (EOf e n Nothing) = Node ("." <> getIdent n) [toTree e]
  toTree (EOf e n (Just es)) =
    Node ("." <> getIdent n <> "()") [toTree e, listToTree "args" es]


instance Parsable Expr where
  parser = asum
    [ EInt <$> parser
    , EReal <$> parser
    , EChar <$> parser
    , EStr <$> parser
    , EBool <$> parser
    , kw "this" $> EThis
    , EVar <$> parser <*> optional (parens (maybeCommas parser))
    , EParens <$> parens parser
    ]
