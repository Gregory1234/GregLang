{-# LANGUAGE TemplateHaskell, DerivingVia, DeriveFunctor, DeriveFoldable,
  DeriveTraversable #-}

module GL.Data.SyntaxTree
  ( module GL.Data.SyntaxTree
  , module GL.Data.SyntaxTree.Stat
  , module GL.Data.SyntaxTree.Expr
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
  toTree (AST p i f c) = Node
    "AST"
    [ toTree $ "package" ++ show p
    , listToTree "imports" i
    , listToTree "funs"    f
    , listToTree "classes" c
    ]

instance Treeable GLImport where
  toTree = toTree . show

instance IsType t => Treeable (GLClass t) where
  toTree (GLClass n f) = listToTree ("class " ++ show n) f

instance IsType t => Treeable (GLFun t) where
  toTree (GLFun t n as s) = listToTree
    ("fun " ++ showTypeShow t n)
    (listToTree "args" (uncurry showTypeShow <$> as) : map toTree s)

instance Show GLPackage where
  show (GLPackage s) = intercalate "." s

instance Show GLImport where
  show (GLImport p) = "import" ++ show p

data AST t = AST
  { _astPackage :: GLPackage
  , _astImports :: [GLImport]
  , _astFunctions :: [GLFun t]
  , _astClasses :: [GLClass t]
  } deriving stock (Functor,Foldable,Traversable)
    deriving Show via (PrettyTree (AST t))

newtype GLPackage = GLPackage { _packagePath :: [String] } deriving Eq

newtype GLImport = GLImport { _importPackage :: GLPackage }

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
makeLenses ''GLPackage
makeLenses ''GLImport
makeLenses ''GLClass
makeLenses ''GLFun

importPath :: Iso' GLImport [String]
importPath = importPackage . packagePath
