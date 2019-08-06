{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module GL.Data.SyntaxTree where

import Data.Functor
import Data.List
import Data.Tree
import GL.Data.TH
import GL.Type
import GL.Utils
import qualified Text.ParserCombinators.ReadP as RP
import Text.Read

$(genEnum
    "SetOp"
    "Op"
    (mkEnumList
       [ "AddSet +="
       , "SubSet -="
       , "MulSet *="
       , "DivSet /="
       , "ModSet %="
       , "AndSet &&="
       , "OrSet ||="
       , "XorSet ^^="
       , "BAndSet &="
       , "BOrSet |="
       , "BXorSet ^="
       , "Set ="
       ]))

setOps :: [SetOp]
setOps = [minBound .. maxBound]

instance Read SetOp where
  readPrec = foldl1 (<++) $ map (\x -> lift (RP.string (show x)) $> x) setOps

$(genEnum
    "ExprOp"
    "EOp"
    (mkEnumList
       [ "Equal =="
       , "LessEq <="
       , "GreaterEq >="
       , "NotEq !="
       , "Less <"
       , "Greater >"
       , "Add +"
       , "Sub -"
       , "Mul *"
       , "Div /"
       , "Mod %"
       , "And &&"
       , "Or ||"
       , "Xor ^^"
       , "BAnd &"
       , "BOr |"
       , "BXor ^"
       ]))

exprOps :: [ExprOp]
exprOps = [minBound .. maxBound]

instance Read ExprOp where
  readPrec = foldl1 (<++) $ map (\x -> lift (RP.string (show x)) $> x) exprOps

$(genEnum "ExprPrefixOp" "EPfxOp" (mkEnumList ["Not !", "Sub -"]))

exprPrefixOps :: [ExprPrefixOp]
exprPrefixOps = [minBound .. maxBound]

instance Read ExprPrefixOp where
  readPrec =
    foldl1 (<++) $ map (\x -> lift (RP.string (show x)) $> x) exprPrefixOps

data AST t =
  AST [GLImport] (GLClass t)

newtype GLImport =
  GLImport [String]

data GLClass t =
  GLClass String [GLFun t]

data GLFun t =
  GLFun t String [(t, String)] [GLStat t]

data GLStat t
  = SIf (GLExpr t) (GLStat t) (Maybe (GLStat t))
  | SFor (GLStat t) (GLExpr t) (GLStat t) (GLStat t)
  | SWhile (GLExpr t) (GLStat t)
  | SDoWhile (GLExpr t) (GLStat t)
  | SLet String (GLExpr t)
  | SSet String SetOp (GLExpr t)
  | SReturn (GLExpr t)
  | SBreak
  | SContinue
  | SNoOp
  | SBraces [GLStat t]
  | SExpr (GLExpr t)

data GLExpr t =
  GLExpr t (UntypedExpr (GLExpr t))

data UntypedExpr e
  = EIntLit Integer
  | EFloatLit Double
  | ECharLit Char
  | EStringLit String
  | EOp e ExprOp e
  | EPrefix ExprPrefixOp e
  | EVar String
  | EParen e

instance IsType t => Treeable (AST t) where
  toTree (AST i c) = Node "AST" [listToTree "imports" i, toTree c]

instance Treeable GLImport where
  toTree = toTree . show

instance IsType t => Treeable (GLClass t) where
  toTree (GLClass n f) = listToTree ("class " ++ n) f

instance IsType t => Treeable (GLFun t) where
  toTree (GLFun t n as s) =
    let (Node x y) = listToTree ("fun " ++ showType t n) s
     in Node x (listToTree "args" (uncurry showType <$> as) : y)

instance IsType t => Treeable (GLStat t) where
  toTree (SIf e s Nothing) = Node "if" [toTree e, Node "then" [toTree s]]
  toTree (SIf e s1 (Just s2)) =
    Node "if" [toTree e, Node "then" [toTree s1], Node "else" [toTree s2]]
  toTree (SFor s1 e s2 s3) =
    Node "for" [toTree s1, toTree e, toTree s2, Node "do" [toTree s3]]
  toTree (SWhile e s) = Node "while" [toTree e, Node "do" [toTree s]]
  toTree (SDoWhile e s) = Node "do" [toTree s, Node "while" [toTree e]]
  toTree (SLet n e) = Node ("let " ++ n ++ " =") [toTree e]
  toTree (SSet n op e) = Node (n ++ " " ++ show op) [toTree e]
  toTree (SReturn e) = Node "return" [toTree e]
  toTree SBreak = toTree "break"
  toTree SContinue = toTree "continue"
  toTree SNoOp = toTree "no op"
  toTree (SBraces s) = listToTree "braces" s
  toTree (SExpr e) = toTree e

instance IsType t => Treeable (GLExpr t) where
  toTree (GLExpr t e) = showTypeTree t (toTree e)

instance Treeable e => Treeable (UntypedExpr e) where
  toTree (EIntLit i) = toTree $ show i
  toTree (EFloatLit f) = toTree $ show f
  toTree (EStringLit s) = toTree $ show s
  toTree (ECharLit c) = toTree $ show c
  toTree (EOp e1 op e2) = Node ("operator " ++ show op) [toTree e1, toTree e2]
  toTree (EPrefix op e) = Node ("operator " ++ show op) [toTree e]
  toTree (EVar e) = toTree e
  toTree (EParen e) = Node "parens" [toTree e]

instance IsType t => Show (AST t) where
  show = treeShow

instance Show GLImport where
  show (GLImport s) = intercalate "." s

instance IsType t => Show (GLClass t) where
  show = treeShow

instance IsType t => Show (GLFun t) where
  show = treeShow

instance IsType t => Show (GLStat t) where
  show = treeShow

instance IsType t => Show (GLExpr t) where
  show = treeShow

instance Treeable e => Show (UntypedExpr e) where
  show = treeShow
