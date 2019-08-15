{-# LANGUAGE DerivingVia, DeriveFunctor, DeriveFoldable, DeriveTraversable,
  GeneralizedNewtypeDeriving #-}
module GL.Data.SyntaxTree.Stat
  ( GLStat(..)
  , SetOp(..)
  , statExprs
  )
where

import           GL.Utils
import           GL.Type
import           GL.Data.Ident
import           GL.Data.SyntaxTree.Expr
import           Control.Lens                   ( Traversal' )
import           Text.Read
import qualified Text.ParserCombinators.ReadP  as RP
import           Data.String
import           Data.List
import           Data.Maybe

newtype SetOp = SetOp { unSetOp :: String }
  deriving newtype (Eq, Ord, IsString)
  deriving Show via ClearShow

setOps :: [String]
setOps =
  ["+=", "-=", "*=", "/=", "%=", "&&=", "||=", "^^=", "&=", "|=", "^=", "="]

instance Read SetOp where
  readPrec = SetOp <$> foldl1 (<++) (map (lift . RP.string) setOps)

instance Enum SetOp where
  toEnum   = SetOp . (setOps !!)
  fromEnum = fromJust . (`elemIndex` setOps) . unSetOp

instance Bounded SetOp where
  minBound = SetOp (head setOps)
  maxBound = SetOp (last setOps)

data GLStat t
  = SIf (GLExpr t) (GLStat t) (Maybe (GLStat t))
  | SFor (GLStat t) (GLExpr t) (GLStat t) (GLStat t)
  | SWhile (GLExpr t) (GLStat t)
  | SDoWhile (GLExpr t) (GLStat t)
  | SLet t Ident (GLExpr t)
  | SSet Ident SetOp (GLExpr t)
  | SReturn (GLExpr t)
  | SBreak
  | SContinue
  | SNoOp
  | SBraces [GLStat t]
  | SExpr (GLExpr t)
  deriving stock (Functor,Foldable,Traversable)
  deriving Show via (PrettyTree (GLStat t))

statExprs :: Traversal' (GLStat t) (GLExpr t)
statExprs f (SIf e s1 s2) =
  SIf <$> f e <*> statExprs f s1 <*> traverse (statExprs f) s2
statExprs f (SFor s1 e s2 s3) =
  SFor <$> statExprs f s1 <*> f e <*> statExprs f s2 <*> statExprs f s3
statExprs f (SWhile   e s) = SWhile <$> f e <*> statExprs f s
statExprs f (SDoWhile e s) = SDoWhile <$> f e <*> statExprs f s
statExprs f (SLet t n  e ) = SLet t n <$> f e
statExprs f (SSet n op e ) = SSet n op <$> f e
statExprs f (SReturn e   ) = SReturn <$> f e
statExprs _ SBreak         = pure SBreak
statExprs _ SContinue      = pure SContinue
statExprs _ SNoOp          = pure SNoOp
statExprs f (SBraces xs)   = SBraces <$> traverse (statExprs f) xs
statExprs f (SExpr   e )   = SExpr <$> f e

instance IsType t => Treeable (GLStat t) where
  toTree (SIf e s Nothing) = Node "if" [toTree e, Node "then" [toTree s]]
  toTree (SIf e s1 (Just s2)) =
    Node "if" [toTree e, Node "then" [toTree s1], Node "else" [toTree s2]]
  toTree (SFor s1 e s2 s3) =
    Node "for" [toTree s1, toTree e, toTree s2, Node "do" [toTree s3]]
  toTree (SWhile   e s) = Node "while" [toTree e, Node "do" [toTree s]]
  toTree (SDoWhile e s) = Node "do" [toTree s, Node "while" [toTree e]]
  toTree (SLet t n  e ) = Node ("let " ++ showTypeShow t n ++ " =") [toTree e]
  toTree (SSet n op e ) = Node (show n ++ " " ++ show op) [toTree e]
  toTree (SReturn e   ) = Node "return" [toTree e]
  toTree SBreak         = toTree "break"
  toTree SContinue      = toTree "continue"
  toTree SNoOp          = toTree "no op"
  toTree (SBraces s)    = listToTree "braces" s
  toTree (SExpr   e)    = toTree e
