{-# LANGUAGE TemplateHaskell, DerivingVia, DeriveFunctor, DeriveFoldable,
  DeriveTraversable #-}
module GL.Data.SyntaxTree.Expr where

import           GL.Utils
import           GL.Type
import           GL.Data.Ident
import           GL.Data.TH
import           Text.Read
import qualified Text.ParserCombinators.ReadP  as RP
import           Control.Lens
import           Data.Tree
import           Data.Functor

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

data GLExpr t =
    EIntLit t Integer
  | EFloatLit t Double
  | ECharLit t Char
  | EStringLit t String
  | EOp t (GLExpr t) ExprOp (GLExpr t)
  | EPrefix t ExprPrefixOp (GLExpr t)
  | EVar t (Maybe (GLExpr t)) Ident [GLExpr t]
  | EParen t (GLExpr t)
  deriving stock (Functor,Foldable,Traversable)
  deriving Show via (PrettyTree (GLExpr t))

exprType1 :: Lens' (GLExpr t) t
exprType1 f (EIntLit    t i) = flip EIntLit i <$> f t
exprType1 f (EFloatLit  t n) = flip EFloatLit n <$> f t
exprType1 f (EStringLit t s) = flip EStringLit s <$> f t
exprType1 f (ECharLit   t c) = flip ECharLit c <$> f t
exprType1 f (EOp t e1 op e2) = (\t' -> EOp t' e1 op e2) <$> f t
exprType1 f (EPrefix t op e) = (\t' -> EPrefix t' op e) <$> f t
exprType1 f (EVar t d n a  ) = (\t' -> EVar t' d n a) <$> f t
exprType1 f (EParen t e    ) = flip EParen e <$> f t

changeExprType :: t -> GLExpr t -> GLExpr t
changeExprType = set exprType1

getExprType :: GLExpr t -> t
getExprType = view exprType1

instance IsType t => Treeable (GLExpr t) where
  toTree (EIntLit    t i) = showTypeTree t $ toTree $ show i
  toTree (EFloatLit  t f) = showTypeTree t $ toTree $ show f
  toTree (EStringLit t s) = showTypeTree t $ toTree $ show s
  toTree (ECharLit   t c) = showTypeTree t $ toTree $ show c
  toTree (EOp t e1 op e2) =
    showTypeTree t $ Node ("operator " ++ show op) [toTree e1, toTree e2]
  toTree (EPrefix t op e) =
    showTypeTree t $ Node ("operator " ++ show op) [toTree e]
  toTree (EVar t Nothing n []) = showTypeTree t $ toTree n
  toTree (EVar t (Just d) n []) =
    showTypeTree t $ Node (show n) [Node "of" [toTree d]]
  toTree (EVar t Nothing n xs) =
    showTypeTree t $ Node (show n) [listToTree "args" xs]
  toTree (EVar t (Just d) n xs) =
    showTypeTree t $ Node (show n) [Node "of" [toTree d], listToTree "args" xs]
  toTree (EParen t e) = showTypeTree t $ Node "parens" [toTree e]


makePrisms ''GLExpr
