{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module GL.TypeChecker
  ( typeCheck
  )
where

import qualified Data.List.HT                  as L
import           GL.Type
import           GL.Data.SyntaxTree
import           GL.Data.Ident
import           GL.Utils
import           GL.TypeChecker.Context
import           GL.TypeChecker.Solver
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Except
import           Control.Lens            hiding ( Context )
import           Data.Monoid

type RContext = ReaderT (GLPackage, Maybe ClassName) Context

foldMapAp f = (getAp <$>) . foldMapA ((Ap <$>) . f)

typeCheckAST :: AST IType -> Context TypeConstraint
typeCheckAST (AST pn ims funs cs) =
  foldMapAp typeCheckFun funs <&&&> foldMapAp typeCheckClass cs
 where
  typeCheckFun :: GLFun IType -> Context TypeConstraint
  typeCheckFun (GLFun t _ a s) = ctxRaiseAdd a
    $ foldMapAp (\a -> runReaderT (typeCheckStat t a) (pn, Nothing)) s
  typeCheckClass :: GLClass IType -> Context TypeConstraint
  typeCheckClass = undefined

typeCheckStat :: IType -> GLStat IType -> RContext TypeConstraint
typeCheckStat _ (SLet t i e) =
  typeCheckExpr e <&&&> typeEq' t (_exprType e) <* ctxAdd t i
typeCheckStat t (SReturn e) = typeCheckExpr e <&&&> typeEq' t (_exprType e)

typeCheckExpr :: GLExpr IType -> RContext TypeConstraint
typeCheckExpr (GLExpr t (EIntLit _)) = typeEq' t "gl.Int"
typeCheckExpr (GLExpr t (EParen e)) =
  typeCheckExpr e <&&&> typeEq' t (_exprType e)
typeCheckExpr (GLExpr t (EVar Nothing n [])) =
  typeEq t <$> single (ctxGetVars n)
typeCheckExpr (GLExpr t (EOp e1 op e2)) = typeAll'
  [ typeCheckExpr e1
  , typeCheckExpr e2
  , typeAny
  .   fmap (zipTypeEq [t, _exprType e1, _exprType e2] . uncurry (:))
  .   filter ((== 2) . length . snd)
  <$> ctxGetFuns (Ident $ showPP op)
  ]

typeCheck :: AST IType -> Either String (AST GLType)
typeCheck a = runContext (solveConstr a =<< typeCheckAST a) (globalContext a)
