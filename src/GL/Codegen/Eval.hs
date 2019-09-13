{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveFunctor #-}

module GL.Codegen.Eval
  ( module GL.Codegen.Eval
  )
where

import           GL.SyntaxTree
import           GL.Type
import           GL.Ident
import           Control.Monad.Reader
import           Control.Monad.State
import           GL.Context
import           GL.Utils
import           Control.Monad.Free

data GLValue =
    VInt Integer
  | VString String
  | VVoid

type CodegenContext
  = ReaderT (Package, Maybe ClassName) (StateT [[(Ident, GLValue)]] (Free GLIO))

data GLIO n = GLPrintString String n deriving (Functor)

runGregLang :: AST GLType -> IO Integer
runGregLang (AST p _ [f@(GLFun "gl.Int" "main" [] _)] _) =
  foldFree helper
    $   (\(VInt a) -> a)
    <$> evalStateT (runReaderT (runFun f) (p, Nothing)) []
  where helper (GLPrintString s n) = putStrLn s $> n

runFun :: GLFun GLType -> CodegenContext GLValue
runFun (GLFun t n a s) = ctxRaise (runStats s)

runStats :: [GLStat GLType] -> CodegenContext GLValue
runStats (SExpr   e : xs) = runExpr e *> runStats xs
runStats (SReturn e : _ ) = runExpr e

runExpr :: GLExpr GLType -> CodegenContext GLValue
runExpr (GLExpr "gl.Void" (EVar Nothing "println" [s])) = do
  e <- runExpr s
  lift (liftF (GLPrintString (let (VString x) = e in x) VVoid))
runExpr (GLExpr "gl.Int"    (EIntLit    n)) = return (VInt n)
runExpr (GLExpr "gl.String" (EStringLit n)) = return (VString n)
