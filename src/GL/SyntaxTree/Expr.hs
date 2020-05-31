module GL.SyntaxTree.Expr
  ( module GL.SyntaxTree.Expr
  )
where

import           GL.Utils
import           GL.Token.Keyword
import           Data.Char

data Expr
  = EVar Ident
  | EStr String
  | EInt Integer
  | EReal Double
  | EChar Char
  | EBool Bool
  | EThis
  | EBinOp Expr Operator Expr
  | ENot Expr
  | EBNot Expr
  | EIf Expr Expr Expr
  | EOf Expr Ident [Expr]
  deriving (Show)

instance Treeable Expr where
  toTree EThis            = toTree "this"
  toTree (EVar  i       ) = toTree $ "ident " ++ getIdent i
  toTree (EStr  s       ) = toTree $ "string " ++ show s
  toTree (EInt  i       ) = toTree $ "int " ++ show i
  toTree (EReal r       ) = toTree $ "real " ++ show r
  toTree (EChar c       ) = toTree $ "char " ++ show c
  toTree (EBool b       ) = toTree $ "bool " ++ map toLower (show b)
  toTree (EBinOp e1 o e2) = listToTree (map toLower (show o)) [e1, e2]
  toTree (ENot  e       ) = toTree $ listToTree "not" [e]
  toTree (EBNot e       ) = toTree $ listToTree "bnot" [e]
  toTree (EIf e1 e2 e3) =
    Node "if" [toTree e1, listToTree "then" [e2], listToTree "else" [e3]]
  toTree (EOf e n es) = Node (getIdent n) [toTree e, listToTree "args" es]
