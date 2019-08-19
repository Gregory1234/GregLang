{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module GL.TypeChecker
  ( typeCheck
  )
where

import           GL.Type
import           GL.Data.SyntaxTree
import           GL.Data.Ident
import           GL.Utils
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.Except
import           Data.Maybe

data TypeConstraint = TypeEqual IType IType deriving Show

teqt :: MonadWriter [TypeConstraint] m => IType -> IType -> m ()
teqt t1 t2 = tell [TypeEqual t1 t2]
teqn :: MonadWriter [TypeConstraint] m => IType -> GLType -> m ()
teqn t n = teqt t (ConcreteIType n)
teqe :: MonadWriter [TypeConstraint] m => IType -> GLExpr IType -> m ()
teqe t e = teqt t (_exprType e)
eeqn :: MonadWriter [TypeConstraint] m => GLExpr IType -> GLType -> m ()
eeqn e = teqn (_exprType e)

type Ctx t = [CtxLevel t]
type CtxLevel t = [CtxElement t]

data CtxElement t =
    CtxFun GLPackage t Ident [t]
  | CtxMethod GLPackage ClassName t Ident [t]
  | CtxLocal t Ident

data ClassContext = ClassContext
  { currentPackage :: GLPackage
  , currentClassName :: ClassName
  }

ctxGetFuns
  :: (MonadState (Ctx t) m, MonadReader (t, ClassContext) m)
  => Ident
  -> m [(t, [t])]
ctxGetFuns i = do
  (ClassContext p c) <- asks snd
  gets (mapMaybe (helper p c) . concat)
 where
  helper _ _ (CtxFun _ ft fi fa) | i == fi = Just (ft, fa)
  helper p c (CtxMethod fp fc ft fi fa) | p == fp, c == fc, i == fi =
    Just (ft, fa)
  helper _ _ _ = Nothing

ctxGetMethods
  :: (MonadState (Ctx t) m) => GLPackage -> ClassName -> Ident -> m [(t, [t])]
ctxGetMethods p c i = gets (mapMaybe helper . concat)
 where
  helper (CtxMethod fp fc ft fi fa) | p == fp, c == fc, i == fi = Just (ft, fa)
  helper _ = Nothing

ctxGetVars
  :: (MonadState (Ctx t) m, MonadReader (t, ClassContext) m) => Ident -> m [t]
ctxGetVars i = do
  (ClassContext p c) <- asks snd
  gets (mapMaybe (helper p c) . concat)
 where
  helper _ _ (CtxFun _ ft fi []) | i == fi = Just ft
  helper p c (CtxMethod fp fc ft fi []) | p == fp, c == fc, i == fi = Just ft
  helper _ _ (CtxLocal ft fi) | i == fi    = Just ft
  helper _ _ _                             = Nothing

ctxGetVar
  :: ( MonadState (Ctx t) m
     , MonadReader (t, ClassContext) m
     , MonadError String m
     )
  => Ident
  -> m t
ctxGetVar i = liftEither =<< (headEither "Bla" <$> ctxGetVars i)

ctxAdd :: (MonadState (Ctx t) m) => t -> Ident -> m ()
ctxAdd t i = modify (\(x : xs) -> (CtxLocal t i : x) : xs)

ctxRaise :: (MonadState (Ctx t) m) => m a -> m a
ctxRaise m = modify ([] :) *> m <* modify tail

typeInfer :: AST IType -> Either String [TypeConstraint]
typeInfer (AST i (GLClass n f)) = eitherConcat $ helperFun n <$> f
 where
  helperFun :: ClassName -> GLFun IType -> Either String [TypeConstraint]
  helperFun c (GLFun t n a s) =
    eitherConcat
      $   runExcept
      .   execWriterT
      .   flip runReaderT (t, ClassContext (GLPackage []) c)
      .   flip evalStateT ([] : map helperArg a : globalCtx)
      .   helperStat
      <$> s
  helperArg (t, n) = CtxLocal t n
  globalCtx = [[]]
  helperStat (SIf e s1 s2) =
    void
      $  eeqn e "Bool"
      *> helperExpr' e
      *> ctxRaise (helperStat s1)
      *> ctxRaise (traverse helperStat s2)
  helperStat (SFor s1 e s2 s3) = ctxRaise
    (  helperStat s1
    *> eeqn e "Bool"
    *> helperExpr' e
    *> helperStat s2
    *> helperStat s3
    )
  helperStat (SWhile   e s) = helperExpr' e *> ctxRaise (helperStat s)
  helperStat (SDoWhile e s) = ctxRaise (helperStat s) *> helperExpr' e
  helperStat (SLet t n   e) = helperExpr' e *> ctxAdd t n
  helperStat (SSet n "=" e) = (flip teqe e =<< ctxGetVar n) *> helperExpr' e
  helperStat (SSet _ _   e) = helperExpr' e
  helperStat (SReturn e   ) = (flip teqe e =<< asks fst) *> helperExpr' e
  helperStat SBreak         = pure ()
  helperStat SContinue      = pure ()
  helperStat SNoOp          = pure ()
  helperStat (SBraces s)    = void $ ctxRaise $ mapM helperStat s
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

solveConstraints :: AST IType -> [TypeConstraint] -> Either String (AST GLType)
solveConstraints _ c = Left (show c)

typeCheck :: AST IType -> Either String (AST GLType)
typeCheck a = solveConstraints a =<< typeInfer a
