{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections #-}

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

data TypeConstraint = TypeEqual IType IType deriving (Eq)

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
  | CtxField GLPackage ClassName t Ident
  | CtxMethod GLPackage ClassName t Ident [t]
  | CtxLocal t Ident

data ClassContext = ClassContext GLPackage (Maybe ClassName)

ctxGetFuns
  :: (MonadState (Ctx t) m, MonadReader (v, ClassContext) m)
  => Ident
  -> m [(t, [t])]
ctxGetFuns i = do
  (ClassContext p c) <- asks snd
  gets (mapMaybe (helper p c) . concat)
 where
  helper _ _ (CtxFun _ ft fi fa) | i == fi = Just (ft, fa)
  helper p (Just c) (CtxMethod fp fc ft fi fa) | p == fp, c == fc, i == fi =
    Just (ft, fa)
  helper _ _ _ = Nothing

ctxGetMethods
  :: MonadState (Ctx t) m => GLPackage -> ClassName -> Ident -> m [(t, [t])]
ctxGetMethods p c i = gets (mapMaybe helper . concat)
 where
  helper (CtxMethod fp fc ft fi fa) | p == fp, c == fc, i == fi = Just (ft, fa)
  helper _ = Nothing

ctxGetVars
  :: (MonadState (Ctx t) m, MonadReader (v, ClassContext) m) => Ident -> m [t]
ctxGetVars i = do
  (ClassContext p c) <- asks snd
  gets (mapMaybe (helper p c) . concat)
 where
  helper _ _ (CtxFun _ ft fi []) | i == fi = Just ft
  helper p (Just c) (CtxMethod fp fc ft fi []) | p == fp, c == fc, i == fi =
    Just ft
  helper _ _ (CtxLocal ft fi) | i == fi = Just ft
  helper _ _ _                          = Nothing

ctxGetVar
  :: ( MonadState (Ctx t) m
     , MonadReader (v, ClassContext) m
     , MonadError String m
     )
  => Ident
  -> m t
ctxGetVar i =
  liftEither
    =<< (headEither ("Couldn't fine the variable " ++ showPP i) <$> ctxGetVars i
        )

ctxGetFields :: MonadState (Ctx t) m => GLPackage -> ClassName -> Ident -> m [t]
ctxGetFields p c i = gets (mapMaybe helper . concat)
 where
  helper (CtxMethod fp fc ft fi []) | p == fp, c == fc, i == fi = Just ft
  helper (CtxField fp fc ft fi) | p == fp, c == fc, i == fi = Just ft
  helper _ = Nothing

ctxAdd :: (MonadState (Ctx t) m) => t -> Ident -> m ()
ctxAdd t i = modify (\(x : xs) -> (CtxLocal t i : x) : xs)

ctxRaise :: (MonadState (Ctx t) m) => m a -> m a
ctxRaise m = modify ([] :) *> m <* modify tail

globalContext :: AST t -> Ctx t
globalContext (AST pn _ f cs) =
  (helperFun Nothing <$> f) : (helperClass <$> cs)
 where
  helperClass (GLClass cn fs ms) =
    (helperField cn <$> fs) ++ (helperFun (Just cn) <$> ms)
  helperField cn (GLField t n _) = CtxField pn cn t n
  helperFun Nothing   (GLFun t n a _) = CtxFun pn t n (fst <$> a)
  helperFun (Just cn) (GLFun t n a _) = CtxMethod pn cn t n (fst <$> a)

typeInfer :: AST IType -> Either String [TypeConstraint]
typeInfer ast@(AST pn _ f cs) =
  eitherConcat
    . concat
    $ ((helperFun pn Nothing <$> f) : (helperClass pn <$> cs))
 where
  helperClass p (GLClass n fs ms) =
    (helperField p n <$> fs) ++ (helperFun p (Just n) <$> ms)
  helperField p c (GLField t n e) =
    runExcept
      $  execWriterT
      $  flip runReaderT ((), ClassContext p (Just c))
      $  flip evalStateT ([] : globalCtx)
      $  void
      $  traverse (teqe t)    e
      *> traverse helperExpr' e
  helperFun p c (GLFun t _ a s) =
    eitherConcat
      $   runExcept
      .   execWriterT
      .   flip runReaderT (t, ClassContext p c)
      .   flip evalStateT ([] : (uncurry CtxLocal <$> a) : globalCtx)
      .   helperStat
      <$> s
  globalCtx = globalContext ast
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
  helperStat (SLet t n   e) = teqe t e *> helperExpr' e *> ctxAdd t n
  helperStat (SSet n "=" e) = (flip teqe e =<< ctxGetVar n) *> helperExpr' e
  helperStat (SSet _ _   e) = helperExpr' e
  helperStat (SReturn e   ) = (flip teqe e =<< asks fst) *> helperExpr' e
  helperStat SBreak         = pure ()
  helperStat SContinue      = pure ()
  helperStat SNoOp          = pure ()
  helperStat (SBraces s)    = void $ ctxRaise $ mapM helperStat s
  helperStat (SExpr   e)    = helperExpr' e
  helperExpr' (GLExpr t u) = helperExpr t u
  helperExpr t (EIntLit    _     ) = teqn t "Int"
  helperExpr t (EFloatLit  _     ) = teqn t "Float"
  helperExpr t (ECharLit   _     ) = teqn t "Char"
  helperExpr t (EStringLit _     ) = teqn t "String"
  helperExpr _ (EOp e1 _ e2      ) = helperExpr' e1 *> helperExpr' e2
  helperExpr _ (EPrefix _ e      ) = helperExpr' e
  helperExpr t (EVar Nothing n []) = teqt t =<< ctxGetVar n
  helperExpr _ (EVar e _ es) =
    void $ traverse helperExpr' e *> traverse helperExpr' es
  helperExpr t (EParen e) = teqe t e *> helperExpr' e

solveConstraints :: [TypeConstraint] -> Either String [GLType]
solveConstraints c =
  maybeToEither "couldn't solve for a type"
    .   sequence
    .   snd
    =<< tryMatchingAll
    =<< tryTillStableM tryMatchingAll (c, [])
 where
  tryMatchingAll
    :: ([TypeConstraint], [Maybe GLType])
    -> Either String ([TypeConstraint], [Maybe GLType])
  tryMatchingAll ([]    , ys) = Right ([], ys)
  tryMatchingAll (x : xs, ys) = do
    (b, ys') <- tryMatchingOne x ys
    if b
      then tryMatchingAll (xs, ys')
      else first (x :) <$> tryMatchingAll (xs, ys)
  tryMatchingOne (TypeEqual (ConcreteIType n) (ConcreteIType m))
    | n == m = Right . (True, )
    | otherwise = const
    $ Left ("Couldn't match types " ++ showPP n ++ " and " ++ showPP m)
  tryMatchingOne (TypeEqual (ConcreteIType n) (NumberIType a)) =
    Right . (True, ) . setAt a (Just n)
  tryMatchingOne (TypeEqual (NumberIType a) (ConcreteIType n)) =
    Right . (True, ) . setAt a (Just n)
  tryMatchingOne (TypeEqual (NumberIType a) (NumberIType b)) = do
    v <- ask
    case getAt a v of
      Nothing -> Right . case getAt b v of
        Nothing   -> (False, )
        (Just b') -> (True, ) . setAt a (Just b')
      (Just a') -> case getAt b v of
        Nothing -> Right . (True, ) . setAt b (Just a')
        (Just b') ->
          const $ Left
            ("Couldn't match types " ++ showPP a' ++ " and " ++ showPP b')


mapSolved :: [GLType] -> AST IType -> AST GLType
mapSolved l = fmap helper
 where
  helper (NumberIType   i) = l !! fromIntegral i
  helper (ConcreteIType t) = t

typeCheck :: AST IType -> Either String (AST GLType)
typeCheck a = flip mapSolved a <$> (typeInfer >=> solveConstraints) a
