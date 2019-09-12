{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module GL.Codegen.Eval
  ( module GL.Codegen.Eval
  )
where

import           GL.SyntaxTree
import           GL.Type
import           GL.Ident
import           Control.Monad.Reader
import           Control.Monad.State

data GLValue =
    VInt Integer
  | VFun (GLValue -> GLValue)
  | VVoid

type CodegenContext
  = ReaderT (Package, Maybe ClassName) (StateT [[(Ident, GLValue)]] IO)

codegenCtxRaise :: MonadState [[a]] m => m b -> m b
codegenCtxRaise = codegenCtxRaiseAdd []
codegenCtxRaiseAdd :: MonadState [[a]] m => [a] -> m b -> m b
codegenCtxRaiseAdd a m = modify (a :) *> m <* modify tail

runGregLang :: AST GLType -> IO Integer
runGregLang (AST p _ [f@(GLFun "gl.Int" "main" [] _)] _) = do
  (VInt a) <- evalStateT (runReaderT (runFun f) (p, Nothing)) []
  return a

runFun :: GLFun GLType -> CodegenContext GLValue
runFun (GLFun t n _ s) = codegenCtxRaiseAdd [] (runStats s)

runStats :: [GLStat GLType] -> CodegenContext GLValue
runStats (SReturn e : _) = runExpr e

runExpr :: GLExpr GLType -> CodegenContext GLValue
runExpr (GLExpr "gl.Int" (EIntLit n)) = return (VInt n)
