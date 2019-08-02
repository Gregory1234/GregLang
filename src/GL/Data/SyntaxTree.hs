{-# LANGUAGE ScopedTypeVariables #-}

module GL.Data.SyntaxTree where

import Data.List
import Data.Proxy
import Data.Tree
import GL.Type
import GL.Utils

data AST t =
  AST [GLImport] (GLClass t)

newtype GLImport =
  GLImport [String]

data GLClass t =
  GLClass String [GLFun t]

data GLFun t =
  GLFun (t String) [t String] [GLStat t]

data GLStat t
  = SIf (GLExpr t) (GLStat t)
  | SIfElse (GLExpr t) (GLStat t) (GLStat t)
  | SFor (GLStat t) (GLExpr t) (GLStat t) (GLStat t)
  | SWhile (GLExpr t) (GLStat t)
  | SLet String (GLExpr t)
  | SBraces [GLStat t]
  | SExpr (GLExpr t)

newtype GLExpr t =
  GLExpr (t (UntypedExpr (GLExpr t)))

data UntypedExpr e
  = EIntLit Integer
  | EFloatLit Double
  | EParen e

instance TypeFunctor t => Treeable (AST t) where
  toTree (AST i c) = Node "AST" [listToTree "imports" i, toTree c]

instance Treeable GLImport where
  toTree = toTree . show

instance TypeFunctor t => Treeable (GLClass t) where
  toTree (GLClass n f) = listToTree ("class " ++ n) f

instance TypeFunctor t => Treeable (GLFun t) where
  toTree (GLFun n as s) =
    let (Node x y) = listToTree ("fun " ++ showAnnotatedString n) s
     in Node x (listToTree "args" (showAnnotatedString <$> as) : y)

instance TypeFunctor t => Treeable (GLStat t) where
  toTree (SIf e s) = Node "if" [toTree e, Node "then" [toTree s]]
  toTree (SIfElse e s1 s2) =
    Node "if" [toTree e, Node "then" [toTree s1], Node "else" [toTree s2]]
  toTree (SFor s1 e s2 s3) =
    Node "for" [toTree s1, toTree e, toTree s2, Node "do" [toTree s3]]
  toTree (SWhile e s) = Node "while" [toTree e, Node "do" [toTree s]]
  toTree (SLet n e) = Node ("let " ++ n ++ " = ") [toTree e]
  toTree (SBraces s) = listToTree "braces" s
  toTree (SExpr e) = toTree e

instance TypeFunctor t => Treeable (GLExpr t) where
  toTree (GLExpr (t :: t (UntypedExpr (GLExpr t)))) =
    let (Node a l) = toTree $ getValTF t
     in Node (showTypeAnnotation (Proxy :: Proxy t) (getTypeTF t) a) l

instance Treeable e => Treeable (UntypedExpr e) where
  toTree (EIntLit i) = toTree $ show i
  toTree (EFloatLit f) = toTree $ show f
  toTree (EParen e) = Node "parens" [toTree e]

instance TypeFunctor t => Show (AST t) where
  show = treeShow

instance Show GLImport where
  show (GLImport s) = intercalate "." s

instance TypeFunctor t => Show (GLClass t) where
  show = treeShow

instance TypeFunctor t => Show (GLFun t) where
  show = treeShow

instance TypeFunctor t => Show (GLStat t) where
  show = treeShow

instance TypeFunctor t => Show (GLExpr t) where
  show = treeShow

instance Treeable e => Show (UntypedExpr e) where
  show = treeShow
