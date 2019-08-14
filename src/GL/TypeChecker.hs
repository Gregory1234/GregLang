{-# LANGUAGE OverloadedStrings #-}

module GL.TypeChecker
  ( typeCheck
  , IType(..)
  , prepTypeCheck
  )
where

import           GL.Type
import           GL.Data.SyntaxTree
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Maybe
import           Data.Functor
import           Control.Lens                   ( view )

data IType =
    NumberIType Integer
  | ConcreteIType GLType deriving Show

instance IsType IType where
  showType (NumberIType   n) x = x ++ " : <" ++ show n ++ ">"
  showType (ConcreteIType t) x = showType t x

prepTypeCheck :: AST (Maybe GLType) -> AST IType
prepTypeCheck ast = evalState (mapM helper ast) 0
 where
  helper (Just t) = pure $ ConcreteIType t
  helper Nothing  = NumberIType <$> get <* modify (+ 1)

data TypeConstraint = TypeEqual IType IType deriving Show

typeInfer :: AST IType -> [TypeConstraint]
typeInfer = execWriter . (astClass . classFuns . traverse) helperFun
 where
  eq :: IType -> IType -> Writer [TypeConstraint] ()
  eq a b = tell [TypeEqual a b]
  eqn e = eq (view exprType1 e) . ConcreteIType
  eqt = eq . view exprType1
  tqn t = eq t . ConcreteIType
  helperFun f@(GLFun t _ a s) = helperStats t a s $> f
  helperStats r c (SIf e s1 s2 : xs) =
    eqn e (GLType "Bool")
      *> helperStats r c [SExpr e, s1]
      *> helperStats r c (maybeToList s2)
      *> helperStats r c xs
  helperStats r c (SFor s1 e s2 s3 : xs) =
    eqn e (GLType "Bool")
      *> helperStats r c [s1, SExpr e]
      *> helperStats r c [s1, s3]
      *> helperStats r c [s1, s2]
      *> helperStats r c xs
  helperStats r c (SWhile e s : xs) =
    eqn e (GLType "Bool") *> helperStats r c [SExpr e, s] *> helperStats r c xs
  helperStats r c (SDoWhile e s : xs) =
    eqn e (GLType "Bool") *> helperStats r c [s, SExpr e] *> helperStats r c xs
  helperStats r c (SLet t n e : xs) =
    eqt e t *> helperExpr c e *> helperStats r ((t, n) : c) xs
  helperStats r c (SSet _ _ e : xs) = helperExpr c e *> helperStats r c xs
  helperStats r c (SReturn e : xs) =
    eqt e r *> helperExpr c e *> helperStats r c xs
  helperStats r c (SBreak     : xs) = helperStats r c xs
  helperStats r c (SContinue  : xs) = helperStats r c xs
  helperStats r c (SNoOp      : xs) = helperStats r c xs
  helperStats r c (SBraces ys : xs) = helperStats r c (ys ++ xs)
  helperStats r c (SExpr   e  : xs) = helperExpr c e *> helperStats r c xs
  helperStats _ _ []                = return ()
  helperExpr _ (EIntLit    t _) = tqn t (GLType "Int")
  helperExpr _ (EFloatLit  t _) = tqn t (GLType "Float")
  helperExpr _ (EStringLit t _) = tqn t (GLType "String")
  helperExpr _ (ECharLit   t _) = tqn t (GLType "Char")
  helperExpr c (EOp _ e1 _ e2 ) = helperExpr c e1 *> helperExpr c e2
  helperExpr c (EPrefix _ _ e ) = helperExpr c e
  helperExpr c (EVar _ d _ xs) =
    void $ traverse (helperExpr c) d *> traverse (helperExpr c) xs
  helperExpr c (EParen t e) = eqt e t *> helperExpr c e

solveConstraints :: [TypeConstraint] -> AST IType -> Either String (AST GLType)
solveConstraints c = mapM helper
 where
  helper (NumberIType   n) = solve n c
  helper (ConcreteIType n) = Right n
  solve n (TypeEqual (NumberIType a) (NumberIType b) : xs)
    | n == a    = solve b xs
    | n == b    = solve a xs
    | otherwise = solve n xs
  solve n (TypeEqual (NumberIType a) (ConcreteIType b) : xs)
    | n == a    = Right b
    | otherwise = solve n xs
  solve n (TypeEqual (ConcreteIType a) (NumberIType b) : xs)
    | n == b    = Right a
    | otherwise = solve n xs
  solve n (TypeEqual (ConcreteIType a) (ConcreteIType b) : xs)
    | a == b = solve n xs
    | otherwise = Left ("couldn't match types " ++ show a ++ " and " ++ show b)
  solve n [] = Left ("ambigous type " ++ show n)

typeCheck :: AST (Maybe GLType) -> Either String (AST GLType)
typeCheck a =
  let a' = prepTypeCheck a
      c  = typeInfer a'
  in  solveConstraints c a'
