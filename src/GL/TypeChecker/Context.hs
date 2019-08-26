{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module GL.TypeChecker.Context
  ( module GL.TypeChecker.Context
  )
where

import           GL.Data.SyntaxTree
import           GL.Data.Ident
import           GL.Utils
import           Data.Maybe
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Except
import           GL.Type

type Ctx = [[CtxElement]]

data CtxElement =
    CtxFun GLPackage IType Ident [IType]
  | CtxField GLPackage ClassName IType Ident
  | CtxMethod GLPackage ClassName IType Ident [IType]
  | CtxLocal IType Ident

ctxGetFuns
  :: MonadState Ctx m
  => GLPackage
  -> Maybe ClassName
  -> Ident
  -> m [(IType, [IType])]
ctxGetFuns p c i = gets (mapMaybe (helper p c) . concat)
 where
  helper _ _ (CtxFun _ ft fi fa) | i == fi = Just (ft, fa)
  helper p (Just c) (CtxMethod fp fc ft fi fa) | p == fp, c == fc, i == fi =
    Just (ft, fa)
  helper _ _ _ = Nothing

ctxGetMethods
  :: MonadState Ctx m => GLPackage -> ClassName -> Ident -> m [(IType, [IType])]
ctxGetMethods p c i = gets (mapMaybe helper . concat)
 where
  helper (CtxMethod fp fc ft fi fa) | p == fp, c == fc, i == fi = Just (ft, fa)
  helper _ = Nothing

ctxGetVars
  :: MonadState Ctx m => GLPackage -> Maybe ClassName -> Ident -> m [IType]
ctxGetVars p c i = gets (mapMaybe (helper p c) . concat)
 where
  helper _ _ (CtxFun _ ft fi []) | i == fi = Just ft
  helper p (Just c) (CtxMethod fp fc ft fi []) | p == fp, c == fc, i == fi =
    Just ft
  helper _ _ (CtxLocal ft fi) | i == fi = Just ft
  helper _ _ _                          = Nothing

ctxGetFields :: MonadState Ctx m => GLPackage -> ClassName -> Ident -> m [IType]
ctxGetFields p c i = gets (mapMaybe helper . concat)
 where
  helper (CtxMethod fp fc ft fi []) | p == fp, c == fc, i == fi = Just ft
  helper (CtxField fp fc ft fi) | p == fp, c == fc, i == fi = Just ft
  helper _ = Nothing

ctxAdd :: MonadState Ctx m => IType -> Ident -> m ()
ctxAdd t i = modify (\(x : xs) -> (CtxLocal t i : x) : xs)

ctxRaise :: MonadState Ctx m => m a -> m a
ctxRaise m = modify ([] :) *> m <* modify tail

globalContext :: AST IType -> Ctx
globalContext (AST pn _ f cs) =
  (helperFun Nothing <$> f) : (helperClass <$> cs)
 where
  helperClass (GLClass cn fs ms) =
    (helperField cn <$> fs) ++ (helperFun (Just cn) <$> ms)
  helperField cn (GLField t n _) = CtxField pn cn t n
  helperFun Nothing   (GLFun t n a _) = CtxFun pn t n (fst <$> a)
  helperFun (Just cn) (GLFun t n a _) = CtxMethod pn cn t n (fst <$> a)

single :: (MonadError String m) => m [t] -> m t
single m = liftEither =<< (headEither "A search failed" <$> m)

type ContextT m = StateT Ctx (ExceptT String m)
type Context = ContextT Identity

runContextT m c = runExceptT $ evalStateT m c
runContext m c = runExcept $ evalStateT m c
