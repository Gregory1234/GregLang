module GL.TypeChecker
  ( typeCheck
  )
where

import           GL.Type
import           GL.Data.SyntaxTree

data TypeConstraint = TypeEqual IType IType deriving Show

typeInfer :: AST IType -> [TypeConstraint]
typeInfer = undefined

solveConstraints :: [TypeConstraint] -> AST IType -> Either String (AST GLType)
solveConstraints _ = undefined

typeCheck :: AST IType -> Either String (AST GLType)
typeCheck a = solveConstraints (typeInfer a) a
