module GL.Data.SyntaxTree where

import Data.List
import Data.Tree
import GL.Treeable

data AST =
  AST [GLImport] GLClass

data GLImport =
  GLImport [String]

data GLClass =
  GLClass String [GLFun]

data GLFun =
  GLFun String [String] [GLStat]

data GLStat
  = SIf GLExpr GLStat
  | SIfElse GLExpr GLStat GLStat
  | SFor GLStat GLExpr GLStat GLStat
  | SWhile GLExpr GLStat
  | SLet String GLExpr
  | SBraces [GLStat]
  | SExpr GLExpr

data GLExpr
  = EIntLit Integer
  | EFloatLit Double
  | EParen GLExpr

instance Treeable AST where
  toTree (AST i c) = Node "AST" [listToTree "imports" i, toTree c]

instance Treeable GLImport where
  toTree = toTree . show

instance Treeable GLClass where
  toTree (GLClass n f) = listToTree ("class " ++ n) f

instance Treeable GLFun where
  toTree (GLFun n as s) =
    let (Node x y) = listToTree ("fun " ++ n) s
     in Node x (listToTree "args" as : y)

instance Treeable GLStat where
  toTree (SIf e s) = Node "if" [toTree e, Node "then" [toTree s]]
  toTree (SIfElse e s1 s2) =
    Node "if" [toTree e, Node "then" [toTree s1], Node "else" [toTree s2]]
  toTree (SFor s1 e s2 s3) =
    Node "for" [toTree s1, toTree e, toTree s2, Node "do" [toTree s3]]
  toTree (SWhile e s) = Node "while" [toTree e, Node "do" [toTree s]]
  toTree (SLet n e) = Node ("let " ++ n ++ " = ") [toTree e]
  toTree (SBraces s) = listToTree "braces" s
  toTree (SExpr e) = toTree e

instance Treeable GLExpr where
  toTree (EIntLit i) = toTree $ show i
  toTree (EFloatLit i) = toTree $ show i
  toTree (EParen e) = listToTree "parens" [e]

instance Show AST where
  show = treeShow

instance Show GLImport where
  show (GLImport s) = intercalate "." s

instance Show GLClass where
  show = treeShow

instance Show GLFun where
  show = treeShow

instance Show GLStat where
  show = treeShow

instance Show GLExpr where
  show = treeShow
