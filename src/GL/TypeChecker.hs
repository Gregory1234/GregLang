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
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Except
import           Control.Lens            hiding ( Context )

data TypeConstraint = TypeEq IType IType

instance Pretty TypeConstraint where
  showPP (TypeEq a b) = showPP a ++ " ~ " ++ showPP b

tryType :: (MonadError String m, MonadState Ctx m) => IType -> m GLType
tryType (NumIType  _) = throwError "Couldn't get type"
tryType (PartIType t) = GLType <$> single (ctxGetClasses t) <*> pure t
tryType (ConIType  t) = return t

matchIType'
  :: (MonadError String m, MonadState Ctx m) => IType -> GLType -> m GLType
matchIType' a b = matchIType a (ConIType b) >>= tryType

typeCheckAST :: AST IType -> WriterT [TypeConstraint] Context ()
typeCheckAST (AST pn ims funs cs) =
  traverse typeCheckFun funs *> traverse_ typeCheckClass cs
 where
  typeCheckFun :: GLFun IType -> WriterT [TypeConstraint] Context ()
  typeCheckFun (GLFun t _ a s) = traverse_ (typeCheckStat t) s
  typeCheckClass :: GLClass IType -> WriterT [TypeConstraint] Context ()
  typeCheckClass = undefined

typeCheckStat :: IType -> GLStat IType -> WriterT [TypeConstraint] Context ()
typeCheckStat t (SReturn e) = do
  typeCheckExpr e
  tell [TypeEq t (_exprType e)]

typeCheckExpr :: GLExpr IType -> WriterT [TypeConstraint] Context ()
typeCheckExpr (GLExpr t (EIntLit _)) = tell [TypeEq t "gl.Int"]

solveConstraints :: AST IType -> [TypeConstraint] -> Context (AST GLType)
solveConstraints ast xs = traverse (joinFun $ helper <$> list xs) ast
 where
  size = fromMaybe 0 (maximumOf (traverse . _NumIType) ast)
  helper :: [IType] -> IType -> Context GLType
  helper _ (ConIType  t) = return t
  helper _ (PartIType t) = tryType $ PartIType t
  helper l (NumIType  n) = tryType $ l !! fromIntegral n
  blank = map NumIType [0 .. size]
  list :: [TypeConstraint] -> Context [IType]
  list [] = return blank
  list (TypeEq (NumIType n) b : xs) =
    zipWithM matchIType (replace' (NumIType n) b blank) =<< list xs
  list (TypeEq a (NumIType n) : xs) =
    zipWithM matchIType (replace' (NumIType n) a blank) =<< list xs
  list (TypeEq a b : xs) = matchIType a b *> list xs

typeCheck :: AST IType -> Either String (AST GLType)
typeCheck a = runContext
  (solveConstraints a =<< execWriterT (typeCheckAST a))
  (globalContext a)
