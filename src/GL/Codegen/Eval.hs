{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveFunctor,
  TupleSections #-}

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
  | VFloat Double
  | VString String
  | VVoid

getInt :: GLValue -> Integer
getInt (VInt i) = i
runExprInt = fmap getInt . runExpr
getFloat :: GLValue -> Double
getFloat (VFloat i) = i
runExprFloat = fmap getFloat . runExpr
getString :: GLValue -> String
getString (VString i) = i
runExprString = fmap getString . runExpr

type CodegenContext
  = ReaderT (Package, Maybe ClassName) (StateT [[(Ident, GLValue)]] (Free GLIO))

ctxGetVars :: MonadState [[(Ident, GLValue)]] m => Ident -> m [GLValue]
ctxGetVars i = gets (mapMaybe helper . concat)
 where
  helper (fi, fv) | i == fi = Just fv
  helper _                  = Nothing

ctxModify :: MonadState [[(Ident, GLValue)]] m => Ident -> GLValue -> m ()
ctxModify i t = modify helper1
 where
  helper1 []       = []
  helper1 (x : xs) = maybe (x : helper1 xs) (: xs) (helper2 x)
  helper2 []                     = Nothing
  helper2 ((b, _) : xs) | i == b = Just $ (i, t) : fromMaybe xs (helper2 xs)
  helper2 (x : xs)               = (x :) <$> helper2 xs

data GLIO n = GLPrintString String n deriving (Functor)

runGregLang :: AST GLType -> IO Integer
runGregLang (AST p _ [f@(GLFun GLPublic "gl.Int" "main" [] _)] _) =
  foldFree helper
    $   getInt
    <$> evalStateT (runReaderT (runFun f) (p, Nothing)) []
  where helper (GLPrintString s n) = putStrLn s $> n

runFun :: GLFun GLType -> CodegenContext GLValue
runFun (GLFun _ t n a s) = ctxRaise (runStats s)

runStats :: [GLStat GLType] -> CodegenContext GLValue
runStats (SExpr   e    : xs) = runExpr e *> runStats xs
runStats (SReturn e    : _ ) = runExpr e
runStats (SLet _ n   e : xs) = runExpr e >>= ctxAdd . (n, ) >> runStats xs
runStats (SSet n "=" e : xs) = runExpr e >>= ctxModify n >> runStats xs

runExpr :: GLExpr GLType -> CodegenContext GLValue
runExpr (GLExpr "gl.Void" (EVar Nothing "println" [s])) =
  lift . liftF . flip GLPrintString VVoid =<< runExprString s
runExpr (GLExpr t           (EVar Nothing n [])) = unsafeSingle (ctxGetVars n)
runExpr (GLExpr "gl.Int"    (EIntLit    n     )) = return (VInt n)
runExpr (GLExpr "gl.String" (EStringLit n     )) = return (VString n)
runExpr (GLExpr "gl.Int" (EOp e1@(GLExpr "gl.Int" _) "+" e2@(GLExpr "gl.Int" _)))
  = fmap VInt $ (+) <$> runExprInt e1 <*> runExprInt e2
runExpr (GLExpr "gl.Int" (EOp e1@(GLExpr "gl.Int" _) "-" e2@(GLExpr "gl.Int" _)))
  = fmap VInt $ (-) <$> runExprInt e1 <*> runExprInt e2
runExpr (GLExpr "gl.Int" (EOp e1@(GLExpr "gl.Int" _) "*" e2@(GLExpr "gl.Int" _)))
  = fmap VInt $ (*) <$> runExprInt e1 <*> runExprInt e2
runExpr (GLExpr "gl.Int" (EOp e1@(GLExpr "gl.Int" _) "/" e2@(GLExpr "gl.Int" _)))
  = fmap VInt $ div <$> runExprInt e1 <*> runExprInt e2
runExpr (GLExpr "gl.Int" (EOp e1@(GLExpr "gl.Int" _) "%" e2@(GLExpr "gl.Int" _)))
  = fmap VInt $ mod <$> runExprInt e1 <*> runExprInt e2
runExpr (GLExpr "gl.String" (EOp e1@(GLExpr "gl.String" _) "+" e2@(GLExpr "gl.String" _)))
  = fmap VString $ (++) <$> runExprString e1 <*> runExprString e2
