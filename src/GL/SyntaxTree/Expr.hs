{-# LANGUAGE DerivingVia, DeriveFunctor, DeriveFoldable, DeriveTraversable,
  GeneralizedNewtypeDeriving, TemplateHaskell #-}
module GL.AST.Expr
  ( module GL.AST.Expr
  )
where

import           GL.Utils
import           GL.Type
import           GL.Ident
import           Text.Read
import qualified Text.ParserCombinators.ReadP  as RP
import           Control.Lens            hiding ( op )
import           Data.String
import           Data.List

newtype ExprOp = ExprOp { unExprOp :: String }
  deriving newtype (Eq, Ord, IsString)
  deriving Pretty via ClearString

exprOps :: [String]
exprOps = concat
  [ ["==", "<=", ">=", "!=", "<", ">"]
  , ["+", "-", "*", "/", "%"]
  , ["&&", "||", "^^", "&", "|", "^"]
  ]

instance Lexable ExprOp where
  lexAP = ExprOp <$> foldl1 (<++) (map (lift . RP.string) exprOps)

instance Enum ExprOp where
  toEnum   = ExprOp . (exprOps !!)
  fromEnum = fromJust . (`elemIndex` exprOps) . unExprOp

instance Bounded ExprOp where
  minBound = ExprOp (head exprOps)
  maxBound = ExprOp (last exprOps)

newtype ExprPrefixOp = ExprPrefixOp { unExprPrefixOp :: String }
  deriving newtype (Eq, Ord, IsString)
  deriving Pretty via ClearString

exprPrefixOps :: [String]
exprPrefixOps = ["!", "-"]

instance Lexable ExprPrefixOp where
  lexAP = ExprPrefixOp <$> foldl1 (<++) (map (lift . RP.string) exprPrefixOps)

instance Enum ExprPrefixOp where
  toEnum   = ExprPrefixOp . (exprPrefixOps !!)
  fromEnum = fromJust . (`elemIndex` exprPrefixOps) . unExprPrefixOp

instance Bounded ExprPrefixOp where
  minBound = ExprPrefixOp (head exprPrefixOps)
  maxBound = ExprPrefixOp (last exprPrefixOps)

data GLExpr t = GLExpr
  { _exprType :: t
  , _exprUntyped :: GLExprU (GLExpr t)
  } deriving stock (Foldable,Traversable,Functor)

instance IsType t => Treeable (GLExpr t) where
  toTree (GLExpr t e) = showTypeTree t (toTree e)

data GLExprU e =
    EIntLit Integer
  | EFloatLit Double
  | ECharLit Char
  | EStringLit String
  | EOp e ExprOp e
  | EPrefix ExprPrefixOp e
  | EVar (Maybe e) Ident [e]
  | EParen e
  deriving stock (Foldable,Traversable,Functor)
  deriving Pretty via (PrettyTree (GLExprU e))

instance Treeable e => Treeable (GLExprU e) where
  toTree (EIntLit    i      ) = toTree $ showPP i
  toTree (EFloatLit  f      ) = toTree $ showPP f
  toTree (EStringLit s      ) = toTree $ showPP s
  toTree (ECharLit   c      ) = toTree $ showPP c
  toTree (EOp e1 op e2      ) = listToTree ("operator " ++ showPP op) [e1, e2]
  toTree (EPrefix op e      ) = listToTree ("operator " ++ showPP op) [e]
  toTree (EVar Nothing  n []) = toTree n
  toTree (EVar (Just d) n []) = Node (showPP n) [Node "of" [toTree d]]
  toTree (EVar Nothing  n xs) = Node (showPP n) [listToTree "args" xs]
  toTree (EVar (Just d) n xs) =
    Node (showPP n) [Node "of" [toTree d], listToTree "args" xs]
  toTree (EParen e) = Node "parens" [toTree e]

makeLenses ''GLExpr
