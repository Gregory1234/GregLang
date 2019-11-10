{-# LANGUAGE DerivingVia, StandaloneDeriving, PolyKinds, OverloadedStrings,
  GeneralizedNewtypeDeriving #-}
module GregLang.SyntaxTree.Stat
  ( module GregLang.SyntaxTree.Stat
  )
where

import           GL.SyntaxTree
import           GL.Parser
import           GL.Utils

data SNoOp s e t = SNoOp
  deriving Pretty via (PrettyTree (SNoOp s e t))
instance Parsable (SNoOp s e t) where
  parser = kw ";" $> SNoOp
instance Treeable (SNoOp s e t) where
  toTree _ = toTree ("no op" :: String)
instance IsSyntax (SNoOp s e t) where
instance IsStatTyp (SNoOp s) where

newtype SExpr s e t = SExpr (e t)
deriving instance (IsExprTyp e, IsType t) => Pretty (SExpr s e t)
deriving instance (IsExprTyp e, IsType t) => Treeable (SExpr s e t)
deriving instance (IsExprTyp e, IsType t) => Parsable (SExpr s e t)
deriving instance (IsExprTyp e, IsType t) => IsSyntax (SExpr s e t)
instance IsStatTyp (SExpr s) where

newtype SBraces s e t = SBraces [s]
  deriving Pretty via (PrettyTree (SBraces s e t))
instance Treeable s => Treeable (SBraces s e t) where
  toTree (SBraces as) = listToTree "braces" as
instance Parsable s => Parsable (SBraces s e t) where
  parser = SBraces <$> safeBraces
instance IsSyntax s => IsSyntax (SBraces s e t)
instance IsSyntax s => IsStatTyp (SBraces s) where
