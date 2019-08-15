{-# LANGUAGE DerivingVia, DeriveFunctor, DeriveFoldable, DeriveTraversable,
  GeneralizedNewtypeDeriving #-}
module GL.Data.SyntaxTree.Expr
  ( GLExpr(..)
  , ExprOp(..)
  , ExprPrefixOp(..)
  , exprType1
  )
where

import           GL.Utils
import           GL.Type
import           GL.Data.Ident
import           Text.Read
import qualified Text.ParserCombinators.ReadP  as RP
import           Control.Lens                   ( Lens' )
import           Data.String
import           Data.List
import           Data.Maybe

newtype ExprOp = ExprOp { unExprOp :: String }
  deriving newtype (Eq, Ord, IsString)
  deriving Show via ClearShow

exprOps :: [String]
exprOps = concat
  [ ["==", "<=", ">=", "!=", "<", ">"]
  , ["+", "-", "*", "/", "%"]
  , ["&&", "||", "^^", "&", "|", "^"]
  ]

instance Read ExprOp where
  readPrec = ExprOp <$> foldl1 (<++) (map (lift . RP.string) exprOps)

instance Enum ExprOp where
  toEnum   = ExprOp . (exprOps !!)
  fromEnum = fromJust . (`elemIndex` exprOps) . unExprOp

instance Bounded ExprOp where
  minBound = ExprOp (head exprOps)
  maxBound = ExprOp (last exprOps)

newtype ExprPrefixOp = ExprPrefixOp { unExprPrefixOp :: String }
  deriving newtype (Eq, Ord, IsString)
  deriving Show via ClearShow

exprPrefixOps :: [String]
exprPrefixOps = ["!", "-"]

instance Read ExprPrefixOp where
  readPrec =
    ExprPrefixOp <$> foldl1 (<++) (map (lift . RP.string) exprPrefixOps)

instance Enum ExprPrefixOp where
  toEnum   = ExprPrefixOp . (exprPrefixOps !!)
  fromEnum = fromJust . (`elemIndex` exprPrefixOps) . unExprPrefixOp

instance Bounded ExprPrefixOp where
  minBound = ExprPrefixOp (head exprPrefixOps)
  maxBound = ExprPrefixOp (last exprPrefixOps)

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
