{-# LANGUAGE OverloadedStrings #-}

module GL.Codegen.LLVM
  ( codegen
  )
where

import qualified LLVM.AST                      as L
import qualified LLVM.AST.Constant             as C
import qualified LLVM.Pretty                   as L
import qualified Data.Text.Lazy                as T
import qualified LLVM.IRBuilder                as B
import           GL.SyntaxTree
import           GL.Ident
import           GL.Type
import           GL.Utils
import           Data.String
import           Control.Monad.State

codegen :: FilePath -> AST GLType -> String
codegen fp ast@(AST pn _ _ _) =
  T.unpack
    $ L.ppllvm
    $ L.Module (fromString (showPP pn)) (fromString fp) Nothing Nothing
    $ B.execModuleBuilder B.emptyModuleBuilder
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
             (evalStateT (traverse_ codegenStat s) . flip zip (map snd a))
    $> ()

codegenExpr
  :: GLExpr GLType
  -> StateT [(L.Operand, Ident)] (B.IRBuilderT B.ModuleBuilder) L.Operand
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
codegenExpr (GLExpr t (EVar Nothing n [])) = gets (fromJust . lookupInv n)

codegenStat
  :: GLStat GLType
  -> StateT [(L.Operand, Ident)] (B.IRBuilderT B.ModuleBuilder) ()
codegenStat (SLet _ n e) = do
  i <- codegenExpr e
  modify ((i, n) :)
codegenStat (SReturn e) = B.ret =<< codegenExpr e

typeToLLVM :: GLType -> L.Type
typeToLLVM "gl.Void" = L.VoidType
typeToLLVM "gl.Int"  = L.IntegerType 64
