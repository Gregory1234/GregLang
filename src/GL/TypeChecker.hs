{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections #-}

module GL.TypeChecker
  ( typeCheck
  )
where

import qualified Data.List.HT                  as L
import           GL.Type
import           GL.Data.SyntaxTree
import           GL.Data.Ident
import           GL.Utils
import           GL.TypeChecker.Context
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Except
import           Control.Lens            hiding ( Context )

data TypeConstraint = TypeEq IType IType deriving (Eq, Show)

type WContext = WriterT [TypeConstraint] Context
type RWContext = ReaderT (GLPackage, Maybe ClassName) WContext

instance Pretty TypeConstraint where
  showPP (TypeEq a b) = showPP a ++ " ~ " ++ showPP b

tryType :: (MonadError String m, MonadState Ctx m) => IType -> m GLType
tryType (NumIType  _) = throwError "Couldn't get type"
tryType (PartIType t) = GLType <$> single (ctxGetClasses t) <*> pure t
tryType (ConIType  t) = return t

typeCheckAST :: AST IType -> WContext ()
typeCheckAST (AST pn ims funs cs) =
  traverse typeCheckFun funs *> traverse_ typeCheckClass cs
 where
  typeCheckFun :: GLFun IType -> WContext ()
  typeCheckFun (GLFun t _ a s) = ctxRaiseAdd a
    $ traverse_ (\a -> runReaderT (typeCheckStat t a) (pn, Nothing)) s
  typeCheckClass :: GLClass IType -> WContext ()
  typeCheckClass = undefined

typeCheckStat :: IType -> GLStat IType -> RWContext ()
typeCheckStat t (SReturn e) = typeCheckExpr e *> tell [TypeEq t (_exprType e)]

typeCheckExpr :: GLExpr IType -> RWContext ()
typeCheckExpr (GLExpr t (EIntLit _)) = tell [TypeEq t "gl.Int"]
typeCheckExpr (GLExpr t (EParen e)) =
  typeCheckExpr e *> tell [TypeEq t (_exprType e)]
typeCheckExpr (GLExpr t (EVar Nothing n [])) = do
  t' <- single (ctxGetVars n)
  tell [TypeEq t t']

solveConstraints :: AST IType -> [TypeConstraint] -> Context (AST GLType)
solveConstraints ast xs = traverse fun ast
 where
  size = fromIntegral (lengthOf (traverse . _NumIType) ast)
  fun  = joinFun $ helper <$> solveTypes size xs
  helper _ (ConIType  t) = return t
  helper _ (PartIType t) = tryType $ PartIType t
  helper l (NumIType  n) = tryType $ l !! fromIntegral n

couldntMatchTypes :: (Pretty a, Pretty b) => a -> b -> String
couldntMatchTypes a b =
  "Couldn't match types " ++ showPP a ++ " and " ++ showPP b

solveTypes :: Integer -> [TypeConstraint] -> Context [IType]
solveTypes size =
  fmap fst . tryTillStableM solveTypesSingle . (NumIType <$> [0 .. size - 1], )
 where
  solveTypesSingle
    :: ([IType], [TypeConstraint]) -> Context ([IType], [TypeConstraint])
  solveTypesSingle (xs, []    ) = return (xs, [])
  solveTypesSingle (xs, y : ys) = do
    a <- helper y xs
    let xs' = fromMaybe xs a
    let k   = isJust a
    second (if k then id else (y :)) <$> solveTypesSingle (xs', ys)
  helper :: TypeConstraint -> [IType] -> Context (Maybe [IType])
  helper (TypeEq x y) xs =
    let a   = getType x xs
        b   = getType y xs
        xs' = equaliseType a b xs
    in  helperSimilar a b $> toMaybe (xs /= xs') xs'
   where
    getType (NumIType n) l = l !! fromIntegral n
    getType x            _ = x
    equaliseType (NumIType _) (NumIType _) = id
    equaliseType (NumIType n) x            = setAt n x
    equaliseType x            (NumIType n) = setAt n x
    equaliseType _            _            = id
    helperSimilar (NumIType _)  _             = return ()
    helperSimilar _             (NumIType  _) = return ()
    helperSimilar (ConIType  a) (ConIType  b) = helperSame a b
    helperSimilar (PartIType a) (PartIType b) = helperSame a b
    helperSimilar (ConIType  a) (PartIType b) = helperConPart a b
    helperSimilar (PartIType a) (ConIType  b) = helperConPart b a
    helperSame a b = guardError (couldntMatchTypes a b) (a == b)
    helperConPart a b =
      guardError (couldntMatchTypes a b) (typeClass a == b)
        *>  single (ctxGetClasses b)
        >>= guardError (couldntMatchTypes a b)
        .   (typePackage a ==)

typeCheck :: AST IType -> Either String (AST GLType)
typeCheck a = runContext
  (solveConstraints a =<< execWriterT (typeCheckAST a))
  (globalContext a)
