{-# LANGUAGE OverloadedStrings #-}

module GL.Codegen.LLVM
  ( codegen
  )
where

import           LLVM.AST                      as L
import           LLVM.AST.Constant             as L
import           LLVM.Pretty                   as L
import           Data.Text.Lazy                as T
import           LLVM.IRBuilder                as B
import           GL.Data.SyntaxTree
import           GL.Data.Ident
import           GL.Type
import           GL.Utils
import           Data.String
import           Data.Foldable
import           Control.Monad.State

codegen :: FilePath -> AST GLType -> String
codegen fp ast@(AST pn _ _ _) =
  T.unpack
    $ ppllvm
    $ Module (fromString (show pn)) (fromString fp) Nothing Nothing
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
             (bimap typeToLLVM (fromString . show) <$> a)
             (typeToLLVM t)
             (evalStateT $ traverse_ codegenStat s)
    $> ()

codegenExpr
  :: GLExpr GLType
  -> StateT [L.Operand] (B.IRBuilderT B.ModuleBuilder) L.Operand
codegenExpr (GLExpr "Int" (EIntLit i)) =
  return $ L.ConstantOperand $ L.Int 64 i

codegenStat
  :: GLStat GLType -> StateT [L.Operand] (B.IRBuilderT B.ModuleBuilder) ()
codegenStat (SReturn e) = do
  i <- codegenExpr e
  emitTerm (Ret (Just i) [])

typeToLLVM :: GLType -> L.Type
typeToLLVM "Void" = L.VoidType
typeToLLVM "Int"  = L.IntegerType 64
