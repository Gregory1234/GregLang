{-# LANGUAGE TemplateHaskell, DerivingVia, DeriveFunctor, DeriveFoldable,
  DeriveTraversable #-}

module GL.Data.SyntaxTree
  ( AST(..)
  , GLImport(..)
  , GLClass(..)
  , GLFun(..)
  , GLStat(..)
  , GLExpr(..)
  , ExprOp(..)
  , ExprPrefixOp(..)
  , SetOp(..)
  , exprType1
  , statExprs
  , funType
  , funName
  , funArgs
  , funStats
  , className
  , classFuns
  , astImports
  , astClass
  , importPath
  , exprOps
  , exprPrefixOps
  , setOps
  )
where


import           GL.Utils
import           GL.Type
import           GL.Data.Ident
import           GL.Data.SyntaxTree.Stat
import           GL.Data.SyntaxTree.Expr
import           Control.Lens
import           Data.List

instance IsType t => Treeable (AST t) where
  toTree (AST i c) = Node "AST" [listToTree "imports" i, toTree c]

instance Treeable GLImport where
  toTree = toTree . show

instance IsType t => Treeable (GLClass t) where
  toTree (GLClass n f) = listToTree ("class " ++ show n) f

instance IsType t => Treeable (GLFun t) where
  toTree (GLFun t n as s) = listToTree
    ("fun " ++ showTypeShow t n)
    (listToTree "args" (uncurry showTypeShow <$> as) : map toTree s)

instance Show GLImport where
  show (GLImport s) = intercalate "." s

data AST t = AST
  { _astImports :: [GLImport]
  , _astClass :: GLClass t
  } deriving stock (Functor,Foldable,Traversable)
    deriving Show via (PrettyTree (AST t))

newtype GLImport = GLImport { _importPath :: [String] }

data GLClass t = GLClass
  { _className :: ClassName
  , _classFuns :: [GLFun t]
  } deriving stock (Functor,Foldable,Traversable)
    deriving Show via (PrettyTree (GLClass t))

data GLFun t = GLFun
  { _funType :: t
  , _funName :: Ident
  , _funArgs :: [(t, Ident)]
  , _funStats :: [GLStat t]
  } deriving stock (Functor,Foldable,Traversable)
    deriving Show via (PrettyTree (GLFun t))

makeLenses ''AST
makeLenses ''GLImport
makeLenses ''GLClass
makeLenses ''GLFun
