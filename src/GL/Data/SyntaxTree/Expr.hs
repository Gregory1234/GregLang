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
import           Control.Lens            hiding ( op )
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
    ECast t (GLExpr t)
  | EIntLit Integer
  | EFloatLit Double
  | ECharLit Char
  | EStringLit String
  | EOp (GLExpr t) ExprOp (GLExpr t)
  | EPrefix ExprPrefixOp (GLExpr t)
  | EVar (Maybe (GLExpr t)) Ident [GLExpr t]
  | EParen (GLExpr t)
  deriving stock (Functor,Foldable,Traversable)
  deriving Show via (PrettyTree (GLExpr t))

exprType1 :: Traversal' (GLExpr t) t
exprType1 f (ECast t e) = ECast <$> f t <*> pure e
exprType1 f (EParen e ) = exprType1 f e
exprType1 _ x           = pure x

instance IsType t => Treeable (GLExpr t) where
  toTree (ECast t e         ) = showTypeTree t (toTree e)
  toTree (EIntLit    i      ) = toTree $ show i
  toTree (EFloatLit  f      ) = toTree $ show f
  toTree (EStringLit s      ) = toTree $ show s
  toTree (ECharLit   c      ) = toTree $ show c
  toTree (EOp e1 op e2      ) = listToTree ("operator " ++ show op) [e1, e2]
  toTree (EPrefix op e      ) = listToTree ("operator " ++ show op) [e]
  toTree (EVar Nothing  n []) = toTree n
  toTree (EVar (Just d) n []) = Node (show n) [Node "of" [toTree d]]
  toTree (EVar Nothing  n xs) = Node (show n) [listToTree "args" xs]
  toTree (EVar (Just d) n xs) =
    Node (show n) [Node "of" [toTree d], listToTree "args" xs]
  toTree (EParen e) = Node "parens" [toTree e]
