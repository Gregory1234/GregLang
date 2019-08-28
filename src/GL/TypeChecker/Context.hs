{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module GL.TypeChecker.Context
  ( module GL.TypeChecker.Context
  )
where

import           GL.Data.SyntaxTree
import           GL.Data.Ident
import           GL.Utils
import           Control.Monad.State
import           Control.Monad.Except
import           GL.Type

type Ctx = [[CtxElement]]

data CtxElement =
    CtxFun GLPackage IType Ident [IType]
  | CtxField GLType IType Ident
  | CtxMethod GLType IType Ident [IType]
  | CtxLocal IType Ident
  | CtxType GLType

ctxGetFuns
  :: MonadState Ctx m
  => GLPackage
  -> Maybe ClassName
  -> Ident
  -> m [(IType, [IType])]
ctxGetFuns p c i = gets (mapMaybe (helper p c) . concat)
 where
  helper _ _ (CtxFun _ ft fi fa) | i == fi = Just (ft, fa)
  helper p (Just c) (CtxMethod (GLType fp fc) ft fi fa)
    | p == fp, c == fc, i == fi = Just (ft, fa)
  helper _ _ _ = Nothing

ctxGetMethods
  :: MonadState Ctx m => GLPackage -> ClassName -> Ident -> m [(IType, [IType])]
ctxGetMethods p c i = gets (mapMaybe helper . concat)
 where
  helper (CtxMethod (GLType fp fc) ft fi fa) | p == fp, c == fc, i == fi =
    Just (ft, fa)
  helper _ = Nothing

ctxGetVars
  :: MonadState Ctx m => GLPackage -> Maybe ClassName -> Ident -> m [IType]
ctxGetVars p c i = gets (mapMaybe (helper p c) . concat)
 where
  helper _ _ (CtxFun _ ft fi []) | i == fi = Just ft
  helper p (Just c) (CtxMethod (GLType fp fc) ft fi [])
    | p == fp, c == fc, i == fi = Just ft
  helper _ _ (CtxLocal ft fi) | i == fi = Just ft
  helper _ _ _                          = Nothing

ctxGetFields :: MonadState Ctx m => GLType -> Ident -> m [IType]
ctxGetFields c i = gets (mapMaybe helper . concat)
 where
  helper (CtxMethod fc ft fi []) | c == fc, i == fi = Just ft
  helper (CtxField fc ft fi) | c == fc, i == fi = Just ft
  helper _ = Nothing

ctxGetClasses :: MonadState Ctx m => ClassName -> m [GLPackage]
ctxGetClasses c = gets (mapMaybe helper . concat)
 where
  helper (CtxType (GLType tp tc)) | c == tc = Just tp
  helper _ = Nothing

ctxAdd :: MonadState Ctx m => IType -> Ident -> m ()
ctxAdd t i = modify (\(x : xs) -> (CtxLocal t i : x) : xs)

ctxRaise :: MonadState Ctx m => m a -> m a
ctxRaise m = modify ([] :) *> m <* modify tail

ctxRaiseAdd :: MonadState Ctx m => [(IType, Ident)] -> m a -> m a
ctxRaiseAdd xs m = modify (map (uncurry CtxLocal) xs :) *> m <* modify tail

preludeContext :: Ctx
preludeContext = [[CtxType "gl.Int"]]

globalContext :: AST IType -> Ctx
globalContext (AST pn _ f cs) =
  (helperFun Nothing <$> f) : (helperClass <$> cs) ++ preludeContext
 where
  helperClass (GLClass cn fs ms) =
    CtxType (GLType pn cn)
      :  (helperField cn <$> fs)
      ++ (helperFun (Just cn) <$> ms)
  helperField cn (GLField t n _) = CtxField (GLType pn cn) t n
  helperFun Nothing (GLFun t n a _) = CtxFun pn t n (fst <$> a)
  helperFun (Just cn) (GLFun t n a _) =
    CtxMethod (GLType pn cn) t n (fst <$> a)

single :: (MonadError String m) => m [t] -> m t
single m = liftEither =<< (onlyEither "A search failed" <$> m)

type ContextT m = StateT Ctx (ExceptT String m)
type Context = ContextT Identity

runContextT m c = runExceptT $ evalStateT m c
runContext m c = runExcept $ evalStateT m c
