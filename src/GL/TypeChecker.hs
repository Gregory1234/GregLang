{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module GL.TypeChecker
  ( typeCheck
  )
where

import           GL.Type
import           GL.SyntaxTree
import           GL.Ident
import           GL.Utils
import           GL.TypeChecker.Context
import           GL.TypeChecker.Solver
import           Control.Monad.Reader
import           Control.Monad.Writer

type RContext = ReaderT (Package, Maybe ClassName) Context

foldMapAp
  :: (Applicative f1, Applicative f2, Monoid a1, Foldable t)
  => (a2 -> f1 (f2 a1))
  -> t a2
  -> f1 (f2 a1)
foldMapAp f = (getAp <$>) . foldMapA ((Ap <$>) . f)

typeCheckAST :: AST IType -> Context TypeConstraint
typeCheckAST (AST pn _ funs cs) =
  foldMapAp typeCheckFun funs <&&&> foldMapAp typeCheckClass cs
 where
  typeCheckFun :: GLFun IType -> Context TypeConstraint
  typeCheckFun (GLFun _ t _ a s) = ctxRaiseAddLocal a
    $ foldMapAp (\s' -> runReaderT (typeCheckStat t s') (pn, Nothing)) s
  typeCheckClass :: GLClass IType -> Context TypeConstraint
  typeCheckClass = undefined

typeCheckStat :: IType -> GLStat IType -> RContext TypeConstraint
typeCheckStat _ (SLet t i e) =
  typeCheckExpr e <&&&> typeEq' t (_exprType e) <* ctxAddLocal t i
typeCheckStat _ (SSet i "=" e) =
  typeCheckExpr e <&&&> (typeEq' (_exprType e) =<< single (ctxGetVars i))
typeCheckStat t (SReturn e) = typeCheckExpr e <&&&> typeEq' t (_exprType e)
typeCheckStat _ SNoOp       = return []
typeCheckStat t (SBraces l) = ctxRaise $ typeAll' (fmap (typeCheckStat t) l)
typeCheckStat _ (SExpr   e) = typeCheckExpr e


typeCheckExpr :: GLExpr IType -> RContext TypeConstraint
typeCheckExpr (GLExpr t (EIntLit    _)) = typeEq' t "gl.Int"
typeCheckExpr (GLExpr t (EFloatLit  _)) = typeEq' t "gl.Float"
typeCheckExpr (GLExpr t (ECharLit   _)) = typeEq' t "gl.Char"
typeCheckExpr (GLExpr t (EStringLit _)) = typeEq' t "gl.String"
typeCheckExpr (GLExpr t (EParen e)) =
  typeCheckExpr e <&&&> typeEq' t (_exprType e)
typeCheckExpr (GLExpr t (EVar Nothing n [])) =
  typeEq t <$> single (ctxGetVars n)
typeCheckExpr (GLExpr t (EVar Nothing n as)) = typeAll'
  ( (   typeAny
    .   fmap (zipTypeEq (t : map _exprType as) . uncurry (:))
    .   filter ((== length as) . length . snd)
    <$> ctxGetFuns n
    )
  : map typeCheckExpr as
  )
typeCheckExpr (GLExpr t (EOp e1 op e2)) = typeAll'
  [ typeCheckExpr e1
  , typeCheckExpr e2
  , typeAny
  .   fmap (zipTypeEq [t, _exprType e1, _exprType e2] . uncurry (:))
  .   filter ((== 2) . length . snd)
  <$> ctxGetFuns (Ident $ "bin" ++ showPP op)
  ]
typeCheckExpr (GLExpr t (EPrefix op e)) = typeAll'
  [ typeCheckExpr e
  , typeAny
  .   fmap (zipTypeEq [t, _exprType e] . uncurry (:))
  .   filter ((== 1) . length . snd)
  <$> ctxGetFuns (Ident $ "pre" ++ showPP op)
  ]

typeCheck :: AST IType -> Either String (AST GLType)
typeCheck a = runContext (solveConstr a =<< typeCheckAST a) (globalContext a)
