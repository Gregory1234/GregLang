{-# LANGUAGE FlexibleContexts, OverloadedStrings, OverloadedLists #-}

module GL.Context
  ( module GL.Context
  )
where

import           GL.SyntaxTree
import           GL.Ident
import           GL.Utils
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Except
import           GL.Type
import           Data.String

type Ctx = [[CtxElement IType]]
type Ctx' t = [[CtxElement t]]

data CtxElement t =
    CtxFun Package t Ident [t]
  | CtxField GLType t Ident
  | CtxMethod GLType t Ident [t]
  | CtxLocal t Ident
  | CtxType GLType
    deriving (Show)

instance Pretty t => Pretty (CtxElement t) where
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
  :: (MonadReader (Package, Maybe ClassName) m, MonadState Ctx m)
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
  :: (MonadReader (Package, Maybe ClassName) m, MonadState (Ctx' t) m)
  => Ident
  -> m [t]
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

ctxGetClasses :: MonadState Ctx m => ClassName -> m [Package]
ctxGetClasses c = gets (mapMaybe helper . concat)
 where
  helper (CtxType (GLType tp tc)) | c == tc = Just tp
  helper _ = Nothing

ctxAdd :: MonadState (Ctx' t) m => t -> Ident -> m ()
ctxAdd t i = modify (\(x : xs) -> (CtxLocal t i : x) : xs)

ctxModify :: MonadState (Ctx' t) m => t -> Ident -> m ()
ctxModify t i = modify helper1
 where
  helper1 []       = []
  helper1 (x : xs) = maybe (x : helper1 xs) (: xs) (helper2 x)
  helper2 [] = Nothing
  helper2 (CtxLocal a b : xs) | i == b =
    Just $ CtxLocal t i : fromMaybe xs (helper2 xs)
  helper2 (x : xs) = (x :) <$> helper2 xs

ctxRaise :: MonadState (Ctx' t) m => m a -> m a
ctxRaise m = modify ([] :) *> m <* modify tail

ctxRaiseAdd :: MonadState (Ctx' t) m => [(t, Ident)] -> m a -> m a
ctxRaiseAdd xs m = modify (map (uncurry CtxLocal) xs :) *> m <* modify tail

preludeContext :: IsType t => Ctx' t
preludeContext =
  [ [ CtxType "gl.Int"
    , op ["gl"] int "+"
    , op ["gl"] int "-"
    , op ["gl"] int "*"
    , op ["gl"] int "/"
    , op ["gl"] int "%"
    ]
  ]
 where
  op p t n = CtxFun p t n [t, t]
  int = fromType "gl.Int"

globalContext' :: Ctx' t -> AST t -> Ctx' t
globalContext' pre (AST pn _ f cs) =
  (helperFun Nothing <$> f) : (helperClass <$> cs) ++ pre
 where
  helperClass (GLClass cn fs ms) =
    CtxType (GLType pn cn)
      :  (helperField cn <$> fs)
      ++ (helperFun (Just cn) <$> ms)
  helperField cn (GLField t n _) = CtxField (GLType pn cn) t n
  helperFun Nothing (GLFun t n a _) = CtxFun pn t n (fst <$> a)
  helperFun (Just cn) (GLFun t n a _) =
    CtxMethod (GLType pn cn) t n (fst <$> a)

globalContext :: IsType t => AST t -> Ctx' t
globalContext = globalContext' preludeContext

single :: (MonadError String m) => m [t] -> m t
single m = liftEither =<< (onlyEither "A search failed" <$> m)

type ContextT' t m = StateT (Ctx' t) (ExceptT String m)
type ContextT m = StateT Ctx (ExceptT String m)
type Context' t = ContextT' t Identity
type Context = ContextT Identity

runContextT m c = runExceptT $ evalStateT m c
runContext m c = runExcept $ evalStateT m c

ctxFromState :: State (Ctx' t) a -> Context' t a
ctxFromState f = StateT $ return . runState f
