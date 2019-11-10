{-# LANGUAGE DerivingVia, StandaloneDeriving, PolyKinds, OverloadedStrings,
  GeneralizedNewtypeDeriving #-}
module GregLang.SyntaxTree.Stat
  ( module GregLang.SyntaxTree.Stat
  )
where

import           GL.SyntaxTree
import           GL.Parser
import           GL.Utils
import           GregLang.SyntaxTree.Expr
import           GregLang.SyntaxTree.Type

data SNoOp e t = SNoOp
  deriving Pretty via (PrettyTree (SNoOp e t))
instance Parsable (SNoOp e t) where
  parser = kw ";" $> SNoOp
instance Treeable (SNoOp e t) where
  toTree _ = toTree ("no op" :: String)
instance IsSyntax (SNoOp e t) where
instance IsStatTyp SNoOp where

newtype SExpr e t = SExpr (ExprFix e t)
deriving instance (IsExprTyp e, IsType t) => Pretty (SExpr e t)
deriving instance (IsExprTyp e, IsType t) => Treeable (SExpr e t)
deriving instance (IsExprTyp e, IsType t) => Parsable (SExpr e t)
deriving instance (IsExprTyp e, IsType t) => IsSyntax (SExpr e t)
instance IsStatTyp SExpr where
