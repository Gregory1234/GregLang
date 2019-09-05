{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

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
import           Control.Monad.Reader
import           GL.Context

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
  B.function
      (L.mkName n)
      (bimap fromType (fromString . showPP) <$> a)
      (fromType t)
      (\b ->
        fromRight
          <$> runContextT
                (ctxRaiseAdd (zip b (map snd a)) (traverse_ codegenStat s))
                []
      )
    $> ()

codegenExpr
  :: GLExpr GLType
  -> ContextT' L.Operand (B.IRBuilderT B.ModuleBuilder) L.Operand
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
codegenExpr (GLExpr t (EVar Nothing n [])) =
  runReaderT (single (ctxGetVars n)) ([], Nothing)

codegenStat
  :: GLStat GLType -> ContextT' L.Operand (B.IRBuilderT B.ModuleBuilder) ()
codegenStat (SLet _ n e) = do
  i <- codegenExpr e
  ctxAdd i n
codegenStat (SSet n "=" e) = do
  i <- codegenExpr e
  ctxModify i n
codegenStat (SReturn e) = B.ret =<< codegenExpr e

instance IsType L.Type where
  showType s x = x ++ " : " ++ T.unpack (L.ppll s)
  fromType "gl.Void" = L.VoidType
  fromType "gl.Int"  = L.IntegerType 64
