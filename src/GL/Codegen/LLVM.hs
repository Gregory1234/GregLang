{-# LANGUAGE OverloadedStrings #-}

module GL.Codegen.LLVM
  ( codegen
  )
where

import           LLVM.AST                      as L
import           LLVM.AST.Constant             as C
import           LLVM.Pretty                   as L
import           Data.Text.Lazy                as T
import           LLVM.IRBuilder                as B
import           GL.AST
import           GL.Ident
import           GL.Type
import           GL.Utils
import           Data.String
import           Control.Monad.State

codegen :: FilePath -> AST GLType -> String
codegen fp ast@(AST pn _ _ _) =
  T.unpack
    $ ppllvm
    $ Module (fromString (showPP pn)) (fromString fp) Nothing Nothing
    $ execModuleBuilder emptyModuleBuilder
    $ codegen' ast

codegen' :: AST GLType -> B.ModuleBuilder ()
codegen' (AST _ _ fs cs) = do
  traverse_ (codegenFun Nothing) fs
  traverse_ codegenClass         cs

codegenClass :: GLClass GLType -> B.ModuleBuilder ()
codegenClass _ = return ()

codegenFun :: Maybe ClassName -> GLFun GLType -> B.ModuleBuilder ()
codegenFun Nothing (GLFun t (Ident n) a s) =
  B.function (L.mkName n)
             (bimap typeToLLVM (fromString . showPP) <$> a)
             (typeToLLVM t)
             (evalStateT $ traverse_ codegenStat s)
    $> ()

codegenExpr
  :: GLExpr GLType
  -> StateT [L.Operand] (B.IRBuilderT B.ModuleBuilder) L.Operand
codegenExpr (GLExpr "gl.Int" (EIntLit i)) =
  return $ L.ConstantOperand $ C.Int 64 i
codegenExpr (GLExpr _ (EParen e)) = codegenExpr e
codegenExpr (GLExpr "gl.Int" (EOp e1@(GLExpr "gl.Int" _) "+" e2@(GLExpr "gl.Int" _)))
  = B.add <$> codegenExpr e1 =<<* codegenExpr e2
codegenExpr (GLExpr "gl.Int" (EOp e1@(GLExpr "gl.Int" _) "-" e2@(GLExpr "gl.Int" _)))
  = B.sub <$> codegenExpr e1 =<<* codegenExpr e2
codegenExpr (GLExpr "gl.Int" (EOp e1@(GLExpr "gl.Int" _) "*" e2@(GLExpr "gl.Int" _)))
  = B.mul <$> codegenExpr e1 =<<* codegenExpr e2
codegenExpr (GLExpr "gl.Int" (EOp e1@(GLExpr "gl.Int" _) "/" e2@(GLExpr "gl.Int" _)))
  = B.sdiv <$> codegenExpr e1 =<<* codegenExpr e2
codegenExpr (GLExpr "gl.Int" (EOp e1@(GLExpr "gl.Int" _) "%" e2@(GLExpr "gl.Int" _)))
  = B.srem <$> codegenExpr e1 =<<* codegenExpr e2

codegenStat
  :: GLStat GLType -> StateT [L.Operand] (B.IRBuilderT B.ModuleBuilder) ()
codegenStat (SReturn e) = do
  i <- codegenExpr e
  emitTerm (Ret (Just i) [])

typeToLLVM :: GLType -> L.Type
typeToLLVM "gl.Void" = L.VoidType
typeToLLVM "gl.Int"  = L.IntegerType 64
