{-# LANGUAGE TupleSections, FlexibleContexts, OverloadedStrings #-}

module GL.TypeChecker
  ( typeCheck
  )
where

import           GL.Type
import           GL.Data.SyntaxTree
import           GL.Data.Ident
import           GL.Utils
import           GL.TypeChecker.Context
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Except

typeCheckAST :: AST IType -> Context (AST GLType)
typeCheckAST (AST pn ims funs cs) =
  AST pn ims <$> traverse typeCheckFun funs <*> traverse typeCheckClass cs
 where
  typeCheckFun :: GLFun IType -> Context (GLFun GLType)
  typeCheckFun (GLFun t n a s) = do
    (t', a', s') <- ctxRaise (typeCheckStats pn Nothing (t, map fst a, s))
    return $ GLFun t' n (zip a' (map snd a)) s'
  typeCheckClass :: GLClass IType -> Context (GLClass GLType)
  typeCheckClass = undefined

typeCheckStats
  :: GLPackage
  -> Maybe ClassName
  -> (IType, [IType], [GLStat IType])
  -> Context (GLType, [GLType], [GLStat GLType])
typeCheckStats _ _ (rt, at, []) =
  (, , []) <$> tryType rt <*> traverse tryType at
typeCheckStats pn cn (rt, at, x : xs) =
  let a  = typeCheckStat pn cn (rt, at, x)
      as = typeCheckStats pn cn (rt, at, xs)
  in  combine <$> a *>>= as
 where
  combine
    :: MonadError String m
    => (IType, [IType], GLStat GLType)
    -> (GLType, [GLType], [GLStat GLType])
    -> m (GLType, [GLType], [GLStat GLType])
  combine (a, b, c) (d, e, f) =
    let x = tryType =<< matchIType a (ConcreteIType d)
        y = sequenceA $ zipWith matchIType' b e
    in  (, , c : f) <$> x <*> y

typeCheckStat
  :: GLPackage
  -> Maybe ClassName
  -> (IType, [IType], GLStat IType)
  -> Context (IType, [IType], GLStat GLType)
typeCheckStat pn cn (rt, at, SReturn e) = do
  (at', e') <- typeCheckExpr pn cn (at, e)
  rt'       <- matchIType rt (ConcreteIType $ _exprType e')
  return (rt', at', SReturn e')

typeCheckExpr
  :: GLPackage
  -> Maybe ClassName
  -> ([IType], GLExpr IType)
  -> Context ([IType], GLExpr GLType)
typeCheckExpr pn cn (at, GLExpr t (EIntLit n)) =
  (at, ) <$> (GLExpr <$> matchIType' t "Int" <*> pure (EIntLit n))

typeCheck :: AST IType -> Either String (AST GLType)
typeCheck a = runContext (typeCheckAST a) (globalContext a)
