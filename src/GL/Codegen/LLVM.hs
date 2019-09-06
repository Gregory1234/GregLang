{-# LANGUAGE OverloadedStrings, OverloadedLists, FlexibleContexts #-}

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
import           Control.Lens
import           Data.Monoid

type CodegenContext
  = ReaderT
      (Package, Maybe ClassName)
      (StateT [[(Ident, L.Operand)]] (B.IRBuilderT B.ModuleBuilder))

codegen :: FilePath -> AST GLType -> String
codegen fp ast@(AST pn _ _ _) =
  T.unpack
    $ L.ppllvm
    $ L.Module (fromString (showPP pn)) (fromString fp) Nothing Nothing
    $ B.execModuleBuilder B.emptyModuleBuilder
    $ codegen' ast

codegen' :: AST GLType -> B.ModuleBuilder ()
codegen' (AST p _ fs cs) = do
  traverse_ (codegenFun p Nothing) fs
  traverse_ (codegenClass p)       cs

codegenClass :: Package -> GLClass GLType -> B.ModuleBuilder ()
codegenClass _ _ = return ()

codegenFun :: Package -> Maybe ClassName -> GLFun GLType -> B.ModuleBuilder ()
codegenFun p Nothing (GLFun t (Ident n) a s) =
  B.function
      (L.mkName n)
      (bimap fromType (fromString . showPP) <$> a)
      (fromType t)
      ( evalStateT (runReaderT (traverse_ codegenStat s) (p, Nothing))
      . pure
      . zip (map snd a)
      )
    $> ()

codegenExpr :: GLExpr GLType -> CodegenContext L.Operand
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
codegenExpr (GLExpr "gl.Int" (EPrefix "-" e@(GLExpr "gl.Int" _))) =
  B.sub (L.ConstantOperand $ C.Int 64 0) =<< codegenExpr e
codegenExpr (GLExpr t (EVar Nothing n [])) =
  gets (fromJust . getFirst . foldMap (First . lookup n))

codegenCtxRaise :: MonadState [[a]] m => m b -> m b
codegenCtxRaise m = modify ([] :) *> m <* modify tail

codegenStat :: GLStat GLType -> CodegenContext ()
codegenStat (SLet _ n e) = do
  i <- codegenExpr e
  ix 0 %= ((n, i) :)
codegenStat (SSet n "=" e) = modify . helper =<< codegenExpr e
 where
  helper i [] = []
  helper i (x : xs) =
    maybe (x : helper i xs) (: xs) $ replaceOn ((== n) . fst) (n, i) x
codegenStat (SReturn e) = B.ret =<< codegenExpr e
codegenStat SNoOp       = pure ()
codegenStat (SBraces l) = codegenCtxRaise $ traverse_ codegenStat l
codegenStat (SExpr   e) = codegenExpr e $> ()

instance IsType L.Type where
  showType s x = x ++ " : " ++ T.unpack (L.ppll s)
  fromType "gl.Void" = L.VoidType
  fromType "gl.Int"  = L.IntegerType 64
