{-# LANGUAGE TemplateHaskell, DerivingVia, DeriveFunctor, DeriveFoldable,
  DeriveTraversable #-}

module GL.SyntaxTree
  ( module GL.SyntaxTree
  , module GL.SyntaxTree.Stat
  , module GL.SyntaxTree.Expr
  )
where


import           GL.Utils
import           GL.Ident
import           GL.SyntaxTree.Stat
import           GL.SyntaxTree.Expr
import           Control.Lens
import           GL.Type

instance IsType t => Treeable (AST t) where
  toTree (AST p i f c) = Node
    "AST"
    [ toTree $ "package " ++ showPP p
    , listToTree "imports" i
    , listToTree "funs"    f
    , listToTree "classes" c
    ]

instance Treeable GLImport where
  toTree = toTree . showPP

instance IsType t => Treeable (GLClass t) where
  toTree (GLClass n f m) =
    Node ("class " ++ showPP n) (toForest f ++ toForest m)

instance IsType t => Treeable (GLField t) where
  toTree (GLField pb t n (Just e)) =
    Node (showPP pb ++ ' ' : showTypeP t n ++ " =") [toTree e]
  toTree (GLField pb t n Nothing) = toTree (showPP pb ++ ' ' : showTypeP t n)

instance IsType t => Treeable (GLFun t) where
  toTree (GLFun pb t n as s) = listToTree
    (showPP pb ++ " fun " ++ showTypeP t n)
    (listToTree "args" (uncurry showTypeP <$> as) : map toTree s)

instance Pretty GLImport where
  showPP (GLImport p) = "import" ++ showPP p

data AST t = AST
  { _astPackage :: Package
  , _astImports :: [GLImport]
  , _astFunctions :: [GLFun t]
  , _astClasses :: [GLClass t]
  } deriving stock (Functor,Foldable,Traversable)
    deriving Pretty via (PrettyTree (AST t))

newtype GLImport = GLImport { _importPackage :: Package }

data GLClass t = GLClass
  { _className :: ClassName
  , _classFields :: [GLField t]
  , _classFuns :: [GLFun t]
  } deriving stock (Functor,Foldable,Traversable)
    deriving Pretty via (PrettyTree (GLClass t))

data GLVisivility = GLPublic | GLPrivate

instance Pretty GLVisivility where
  showPP GLPublic  = "public"
  showPP GLPrivate = "private"

data GLField t = GLField
  { _fieldVisivility :: GLVisivility
  , _fieldType :: t
  , _fieldName :: Ident
  , _fieldDefault :: Maybe (GLExpr t)
  } deriving stock (Functor,Foldable,Traversable)
    deriving Pretty via (PrettyTree (GLField t))

data GLFun t = GLFun
  { _funVisivility :: GLVisivility
  , _funType :: t
  , _funName :: Ident
  , _funArgs :: [(t, Ident)]
  , _funStats :: [GLStat t]
  } deriving stock (Functor,Foldable,Traversable)
    deriving Pretty via (PrettyTree (GLFun t))

makeLenses ''AST
makeLenses ''GLImport
makeLenses ''GLClass
makeLenses ''GLField
makeLenses ''GLFun
