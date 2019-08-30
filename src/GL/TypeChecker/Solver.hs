{-# LANGUAGE FlexibleContexts #-}

module GL.TypeChecker.Solver
  ( module GL.TypeChecker.Solver
  )
where

import           GL.TypeChecker.Context
import           GL.Utils
import           GL.Data.SyntaxTree
import           GL.Type
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Except
import           Data.List
import           Control.Lens            hiding ( Context )
import           Debug.Trace

traceN s a = trace (s ++ show a) a

type TypeConstraint = [[(IType, IType)]]

typeEq :: IType -> IType -> TypeConstraint
typeEq a b = [[(a, b)]]

typeAll :: [TypeConstraint] -> TypeConstraint
typeAll = foldr typeAnd []

typeAnd :: TypeConstraint -> TypeConstraint -> TypeConstraint
typeAnd = liftA2 (++)

(<&&&>)
  :: Applicative f => f TypeConstraint -> f TypeConstraint -> f TypeConstraint
(<&&&>) = liftA2 typeAnd

tryType :: (MonadError String m, MonadState Ctx m) => IType -> m GLType
tryType (NumIType  _) = throwError "Couldn't get type"
tryType (PartIType t) = GLType <$> single (ctxGetClasses t) <*> pure t
tryType (ConIType  t) = return t

solveConstr :: AST IType -> TypeConstraint -> Context (AST GLType)
solveConstr a t = traverse (joinFun $ helper <$> solver size t) a
 where
  size = fromIntegral (lengthOf (traverse . _NumIType) a) - 1
  helper _ (ConIType  t) = return t
  helper _ (PartIType t) = tryType $ PartIType t
  helper l (NumIType  n) = tryType $ traceN "step 4 " $ l !! fromIntegral n

solver :: Integer -> TypeConstraint -> Context [IType]
solver s =
  single
    . ctxFromState
    . fmap (map $ \a -> helper a =<< [0 .. s])
    . fmap (traceN "step 2 ")
    . fmap (($ []) . foldr (disj . foldr conj pure) (const []))
    . traverse (traverse (\(a, b) -> interchangeStateFun $ a === b))
 where
  helper :: [(IType, IType)] -> Integer -> [IType]
  helper []                              _ = []
  helper ((NumIType _, NumIType _) : xs) m = helper xs m
  helper ((NumIType n, b) : xs) m | n == m = b : helper xs m
  helper ((a, NumIType n) : xs) m | n == m = a : helper xs m
  helper (_ : xs) m                        = helper xs m

disj g1 g2 sc = (concat . transpose) [g1 sc, g2 sc]
conj g1 g2 sc = g1 sc >>= g2
(===) :: IType -> IType -> [(IType, IType)] -> State Ctx [[(IType, IType)]]
(===) a b subs = gets $ \ctx -> case unify ctx a b subs of
  Nothing    -> []
  Just subs' -> return subs'

subst :: [(IType, IType)] -> IType -> IType
subst s x@(NumIType _) = maybe x (subst s) (lookup x s)
subst _ x              = x

unify :: Ctx -> IType -> IType -> [(IType, IType)] -> Maybe [(IType, IType)]
unify ctx lhs rhs subs = (++ subs)
  <$> unifyExpr (subst subs lhs) (subst subs rhs)
 where
  unifyExpr (ConIType a) (ConIType b) | a == b = return []
  unifyExpr (PartIType a) (ConIType (GLType b c))
    | a == c, runContext (single (ctxGetClasses a)) ctx == Right b = return []
  unifyExpr (ConIType (GLType a b)) (PartIType c)
    | b == c, runContext (single (ctxGetClasses c)) ctx == Right a = return []
  unifyExpr (PartIType a) (PartIType b) | a == b = return []
  unifyExpr l@(NumIType _) r                     = return [(l, r)]
  unifyExpr l              r@(NumIType _)        = return [(r, l)]
  unifyExpr _              _                     = Nothing
