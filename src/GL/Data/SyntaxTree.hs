{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DerivingVia,
  StandaloneDeriving, DeriveFunctor #-}

module GL.Data.SyntaxTree where

import           Data.Functor
import           Data.List
import           Data.Tree
import           GL.Data.TH
import           GL.Type
import           GL.Utils
import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read

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
  deriving stock Functor
  deriving Show via (PrettyTree (AST t))

newtype GLImport =
  GLImport [String]

data GLClass t =
  GLClass String [GLFun t]
  deriving stock Functor
  deriving Show via (PrettyTree (GLClass t))

data GLFun t =
  GLFun t String [(t, String)] [GLStat t]
  deriving stock Functor
  deriving Show via (PrettyTree (GLFun t))

data GLStat t
  = SIf (GLExpr t) (GLStat t) (Maybe (GLStat t))
  | SFor (GLStat t) (GLExpr t) (GLStat t) (GLStat t)
  | SWhile (GLExpr t) (GLStat t)
  | SDoWhile (GLExpr t) (GLStat t)
  | SLet t String (GLExpr t)
  | SSet String SetOp (GLExpr t)
  | SReturn (GLExpr t)
  | SBreak
  | SContinue
  | SNoOp
  | SBraces [GLStat t]
  | SExpr (GLExpr t)
  deriving stock Functor
  deriving Show via (PrettyTree (GLStat t))

data GLExpr t =
    EIntLit t Integer
  | EFloatLit t Double
  | ECharLit t Char
  | EStringLit t String
  | EOp t (GLExpr t) ExprOp (GLExpr t)
  | EPrefix t ExprPrefixOp (GLExpr t)
  | EVar t String
  | EParen t (GLExpr t)
  deriving stock Functor
  deriving Show via (PrettyTree (GLExpr t))

changeExprType :: t -> GLExpr t -> GLExpr t
changeExprType t (EIntLit    _ i) = EIntLit t i
changeExprType t (EFloatLit  _ f) = EFloatLit t f
changeExprType t (EStringLit _ s) = EStringLit t s
changeExprType t (ECharLit   _ c) = ECharLit t c
changeExprType t (EOp _ e1 op e2) = EOp t e1 op e2
changeExprType t (EPrefix _ op e) = EPrefix t op e
changeExprType t (EVar   _ n    ) = EVar t n
changeExprType t (EParen _ e    ) = EParen t e

instance IsType t => Treeable (AST t) where
  toTree (AST i c) = Node "AST" [listToTree "imports" i, toTree c]

instance Treeable GLImport where
  toTree = toTree . show

instance IsType t => Treeable (GLClass t) where
  toTree (GLClass n f) = listToTree ("class " ++ n) f

instance IsType t => Treeable (GLFun t) where
  toTree (GLFun t n as s) =
    let (Node x y) = listToTree ("fun " ++ showType t n) s
    in  Node x (listToTree "args" (uncurry showType <$> as) : y)

instance IsType t => Treeable (GLStat t) where
  toTree (SIf e s Nothing) = Node "if" [toTree e, Node "then" [toTree s]]
  toTree (SIf e s1 (Just s2)) =
    Node "if" [toTree e, Node "then" [toTree s1], Node "else" [toTree s2]]
  toTree (SFor s1 e s2 s3) =
    Node "for" [toTree s1, toTree e, toTree s2, Node "do" [toTree s3]]
  toTree (SWhile   e s) = Node "while" [toTree e, Node "do" [toTree s]]
  toTree (SDoWhile e s) = Node "do" [toTree s, Node "while" [toTree e]]
  toTree (SLet t n  e ) = Node ("let " ++ showType t n ++ " =") [toTree e]
  toTree (SSet n op e ) = Node (n ++ " " ++ show op) [toTree e]
  toTree (SReturn e   ) = Node "return" [toTree e]
  toTree SBreak         = toTree "break"
  toTree SContinue      = toTree "continue"
  toTree SNoOp          = toTree "no op"
  toTree (SBraces s)    = listToTree "braces" s
  toTree (SExpr   e)    = toTree e

instance IsType t => Treeable (GLExpr t) where
  toTree (EIntLit    t i) = showTypeTree t $ toTree $ show i
  toTree (EFloatLit  t f) = showTypeTree t $ toTree $ show f
  toTree (EStringLit t s) = showTypeTree t $ toTree $ show s
  toTree (ECharLit   t c) = showTypeTree t $ toTree $ show c
  toTree (EOp t e1 op e2) =
    showTypeTree t $ Node ("operator " ++ show op) [toTree e1, toTree e2]
  toTree (EPrefix t op e) =
    showTypeTree t $ Node ("operator " ++ show op) [toTree e]
  toTree (EVar   t n) = showTypeTree t $ toTree n
  toTree (EParen t e) = showTypeTree t $ Node "parens" [toTree e]

instance Show GLImport where
  show (GLImport s) = intercalate "." s
