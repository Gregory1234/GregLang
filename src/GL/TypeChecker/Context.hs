{-# LANGUAGE FlexibleContexts, OverloadedStrings, OverloadedLists #-}

module GL.TypeChecker.Context
  ( module GL.TypeChecker.Context
  )
where

import           GL.SyntaxTree
import           GL.Ident
import           GL.Utils
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Except
import           GL.Type

type Ctx = [[CtxElement]]

data CtxElement =
    CtxFun GLPackage IType Ident [IType]
  | CtxField GLType IType Ident
  | CtxMethod GLType IType Ident [IType]
  | CtxLocal IType Ident
  | CtxType GLType
    deriving (Show)

instance Pretty CtxElement where
  showPP (CtxFun p t n a) =
    "fun "
      ++ showPP n
      ++ " : "
      ++ showPPList a
      ++ " -> "
      ++ showPP t
      ++ " in "
      ++ showPP p
  showPP (CtxField c t n) =
    "field " ++ showPP n ++ " : " ++ showPP t ++ " in " ++ showPP c
  showPP (CtxFun c t n a) =
    "method "
      ++ showPP n
      ++ " : "
      ++ showPPList a
      ++ " -> "
      ++ showPP t
      ++ " in "
      ++ showPP c
  showPP (CtxLocal t n) = "local " ++ showPP n ++ " : " ++ showPP t
  showPP (CtxType t   ) = "type " ++ showPP t

ctxGetFuns
  :: (MonadReader (GLPackage, Maybe ClassName) m, MonadState Ctx m)
  => Ident
  -> m [(IType, [IType])]
ctxGetFuns i = ask >>= \(p, c) -> gets (mapMaybe (helper p c) . concat)
 where
  helper _ _ (CtxFun _ ft fi fa) | i == fi = Just (ft, fa)
  helper p (Just c) (CtxMethod (GLType fp fc) ft fi fa)
    | p == fp, c == fc, i == fi = Just (ft, fa)
  helper _ _ _ = Nothing

ctxGetMethods :: MonadState Ctx m => GLType -> Ident -> m [(IType, [IType])]
ctxGetMethods c i = gets (mapMaybe helper . concat)
 where
  helper (CtxMethod fc ft fi fa) | c == fc, i == fi = Just (ft, fa)
  helper _ = Nothing

ctxGetVars
  :: (MonadReader (GLPackage, Maybe ClassName) m, MonadState Ctx m)
  => Ident
  -> m [IType]
ctxGetVars i = ask >>= \(p, c) -> gets (mapMaybe (helper p c) . concat)
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
preludeContext =
  [ [ CtxType "gl.Int"
    , op ["gl"] "gl.Int" "+"
    , op ["gl"] "gl.Int" "-"
    , op ["gl"] "gl.Int" "*"
    , op ["gl"] "gl.Int" "/"
    , op ["gl"] "gl.Int" "%"
    ]
  ]
  where op p t n = CtxFun p t n [t, t]

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

ctxFromState :: State Ctx a -> Context a
ctxFromState f = StateT $ return . runState f
