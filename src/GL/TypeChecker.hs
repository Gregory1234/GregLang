{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module GL.TypeChecker
  ( typeCheck
  )
where

import           GL.Type
import           GL.Data.SyntaxTree
import           GL.Data.Ident
import           GL.Utils
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Maybe

data TypeConstraint = TypeEqual IType IType deriving Show

teq :: MonadWriter [TypeConstraint] m => IType -> IType -> m ()
teq t1 t2 = tell [TypeEqual t1 t2]
teqn :: MonadWriter [TypeConstraint] m => IType -> GLType -> m ()
teqn t n = teq t (ConcreteIType n)
teqe :: MonadWriter [TypeConstraint] m => IType -> GLExpr IType -> m ()
teqe t e = teq t (_exprType e)

type ContextT t m = StateT [[(t, Ident)]] (ReaderT t m)

evalContext :: Monad m => ContextT t m a -> [[(t, Ident)]] -> t -> m a
evalContext m = runReaderT . evalStateT m

pushCtx :: Monad m => ContextT t m ()
pushCtx = modify ([] :)

popCtx :: Monad m => ContextT t m ()
popCtx = modify tail

raiseCtx :: Monad m => ContextT t m a -> ContextT t m a
raiseCtx m = pushCtx *> m <* popCtx

addCtx :: Monad m => t -> Ident -> ContextT t m ()
addCtx t n = modify (\(x : xs) -> ((t, n) : x) : xs)

getTypeCtx :: Monad m => Ident -> ContextT t m t
getTypeCtx n = gets $ fromJust . getFirst . foldMap (First . lookupInv n)

typeInfer :: AST IType -> [TypeConstraint]
typeInfer = foldMapOf (astClass . classFuns . traverse) helperFun
 where
  helperFun (GLFun t _ a s) =
    execWriter (evalContext (mapM helperStat s) ([] : a : globalCtx) t)
  globalCtx = [[]]
  helperStat :: GLStat IType -> ContextT IType (Writer [TypeConstraint]) ()
  helperStat (SIf e s1 s2) =
    void $ helperExpr' e *> raiseCtx (helperStat s1) *> raiseCtx
      (traverse helperStat s2)
  helperStat (SWhile   e s) = helperExpr' e *> raiseCtx (helperStat s)
  helperStat (SDoWhile e s) = raiseCtx (helperStat s) *> helperExpr' e
  helperStat (SLet t n   e) = helperExpr' e *> addCtx t n
  helperStat (SSet n "=" e) = (flip teqe e =<< getTypeCtx n) *> helperExpr' e
  helperStat (SSet _ _   e) = helperExpr' e
  helperStat (SReturn e   ) = (flip teqe e =<< ask) *> helperExpr' e
  helperStat SBreak         = pure ()
  helperStat SContinue      = pure ()
  helperStat SNoOp          = pure ()
  helperStat (SBraces s)    = void $ raiseCtx $ mapM helperStat s
  helperStat (SExpr   e)    = helperExpr' e
  helperExpr' (GLExpr t u) = helperExpr t u
  helperExpr t (EIntLit    _) = teqn t "Int"
  helperExpr t (EFloatLit  _) = teqn t "Float"
  helperExpr t (ECharLit   _) = teqn t "Char"
  helperExpr t (EStringLit _) = teqn t "String"
  helperExpr _ (EOp e1 _ e2 ) = helperExpr' e1 *> helperExpr' e2
  helperExpr _ (EPrefix _ e ) = helperExpr' e
  helperExpr _ (EVar e _ es) =
    void $ traverse helperExpr' e *> traverse helperExpr' es
  helperExpr t (EParen e) = teqe t e *> helperExpr' e

solveConstraints :: [TypeConstraint] -> AST IType -> Either String (AST GLType)
solveConstraints c _ = Left (show c)

typeCheck :: AST IType -> Either String (AST GLType)
typeCheck a = solveConstraints (typeInfer a) a
