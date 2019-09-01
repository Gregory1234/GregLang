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

type TypeConstraint = [[(IType, IType)]]

typeEq :: IType -> IType -> TypeConstraint
typeEq a b = [[(a, b)]]

zipTypeEq :: [IType] -> [IType] -> TypeConstraint
zipTypeEq = ((.) . (.)) typeAll (zipWith typeEq)

typeAll :: [TypeConstraint] -> TypeConstraint
typeAll = foldr typeAnd [[]]

typeAll' :: Applicative f => [f TypeConstraint] -> f TypeConstraint
typeAll' = foldr (<&&&>) (pure [[]])

typeAnd :: TypeConstraint -> TypeConstraint -> TypeConstraint
typeAnd = liftA2 (++)

(<&&&>)
  :: Applicative f => f TypeConstraint -> f TypeConstraint -> f TypeConstraint
(<&&&>) = liftA2 typeAnd

typeAny :: [TypeConstraint] -> TypeConstraint
typeAny = foldr typeOr []

typeAny' :: Applicative f => [f TypeConstraint] -> f TypeConstraint
typeAny' = foldr (<|||>) (pure [])

typeOr :: TypeConstraint -> TypeConstraint -> TypeConstraint
typeOr = fairAppend

(<|||>)
  :: Applicative f => f TypeConstraint -> f TypeConstraint -> f TypeConstraint
(<|||>) = liftA2 typeOr

tryType :: (MonadError String m, MonadState Ctx m) => IType -> m GLType
tryType (NumIType  _) = throwError "Couldn't get type"
tryType (PartIType t) = GLType <$> single (ctxGetClasses t) <*> pure t
tryType (ConIType  t) = return t

solveConstr :: AST IType -> TypeConstraint -> Context (AST GLType)
solveConstr a t = traverse (joinFun $ helper <$> solver t) a
 where
  size = fromIntegral (lengthOf (traverse . _NumIType) a) - 1
  helper _ (ConIType  t) = return t
  helper _ (PartIType t) = tryType $ PartIType t
  helper l (NumIType  n) = tryType $ l !! fromIntegral n
  solver =
    single
      . fmap
          (map (\a -> subst a . NumIType <$> [0 .. size]) . ($ []) . foldr
            (appDb fairAppend . foldr1 (>=>))
            (const [])
          )
      . (traverse . traverse . uncurry $ (===))

(===) :: IType -> IType -> Context ([(IType, IType)] -> [[(IType, IType)]])
(===) a b = gets $ \ctx -> maybeToAlt . unify ctx a b
  where unify ctx a b r = (++ r) <$> matchIType ctx (subst r a) (subst r b)

subst :: [(IType, IType)] -> IType -> IType
subst s x@(NumIType _) = maybe x (subst s) (lookup x s)
subst _ x              = x

matchIType _ (ConIType a) (ConIType b) | a == b = return []
matchIType ctx (PartIType a) (ConIType (GLType b c))
  | a == c, runContext (single (ctxGetClasses a)) ctx == Right b = return []
matchIType ctx (ConIType (GLType a b)) (PartIType c)
  | b == c, runContext (single (ctxGetClasses c)) ctx == Right a = return []
matchIType _ (PartIType a) (PartIType b) | a == b = return []
matchIType _ l@(NumIType _) r                     = return [(l, r)]
matchIType _ l              r@(NumIType _)        = return [(r, l)]
matchIType _ _              _                     = Nothing
