{-# LANGUAGE TupleSections #-}

module GL.TypeChecker where

import           GL.Type
import           GL.Data.SyntaxTree
import           Control.Monad.Trans.State

data IType =
    NumberIType Integer
  | ConcreteIType String

instance IsType IType where
  showType (NumberIType   n) x = x ++ " : <" ++ show n ++ ">"
  showType (ConcreteIType n) x = x ++ " : " ++ n

prepTypeCheck :: AST (Maybe String) -> AST IType
prepTypeCheck ast = evalState (helperAST ast) 0
 where
  inc = NumberIType <$> get <* modify (+ 1)
  helperType (Just t) = pure $ ConcreteIType t
  helperType Nothing  = inc
  helperAST (AST i c) = AST i <$> helperClass c
  helperClass (GLClass n f) = GLClass n <$> mapM helperFun f
  helperFun (GLFun t n a s) =
    GLFun
      <$> helperType t
      <*> pure n
      <*> mapM (\(t, n) -> (, n) <$> helperType t) a
      <*> mapM helperStat                          s
  helperStat (SIf e s1 s2) =
    SIf <$> helperExpr e <*> helperStat s1 <*> mapM helperStat s2
  helperStat (SFor s1 e s2 s3) =
    SFor <$> helperStat s1 <*> helperExpr e <*> helperStat s2 <*> helperStat s3
  helperStat (SWhile e s ) = SWhile <$> helperExpr e <*> helperStat s
  helperStat (SLet t n  e) = SLet <$> helperType t <*> pure n <*> helperExpr e
  helperStat (SSet n op e) = SSet n op <$> helperExpr e
  helperStat (SReturn e  ) = SReturn <$> helperExpr e
  helperStat SBreak        = pure SBreak
  helperStat SContinue     = pure SContinue
  helperStat SNoOp         = pure SNoOp
  helperStat (SBraces s)   = SBraces <$> mapM helperStat s
  helperStat (SExpr   e)   = SExpr <$> helperExpr e
  helperExpr :: GLExpr (Maybe String) -> State Integer (GLExpr IType)
  helperExpr (EIntLit    t i) = EIntLit <$> helperType t <*> pure i
  helperExpr (EFloatLit  t f) = EFloatLit <$> helperType t <*> pure f
  helperExpr (EStringLit t s) = EStringLit <$> helperType t <*> pure s
  helperExpr (ECharLit   t c) = ECharLit <$> helperType t <*> pure c
  helperExpr (EOp t e1 op e2) =
    EOp <$> helperType t <*> helperExpr e1 <*> pure op <*> helperExpr e2
  helperExpr (EPrefix t op e) =
    EPrefix <$> helperType t <*> pure op <*> helperExpr e
  helperExpr (EVar   t n) = EVar <$> helperType t <*> pure n
  helperExpr (EParen t e) = EParen <$> helperType t <*> helperExpr e
