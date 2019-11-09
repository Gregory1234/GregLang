{-# LANGUAGE GeneralisedNewtypeDeriving, DerivingVia, StandaloneDeriving,
  TypeApplications, FlexibleContexts, UndecidableInstances,
  FlexibleInstances, OverloadedStrings, PolyKinds, DataKinds #-}
module SyntaxTree
  ( module SyntaxTree
  )
where

import           GL.Utils
import           GL.Parser
import           GL.SyntaxTree
import           GL.Ident
import           GL.Token
import           GHC.Exts
import qualified Text.Megaparsec               as P
import           Control.Applicative
import           Data.List

type UntypedAST
  = AST
      ( FunTyp
          FunSigTyp
          ( StatTypUnion
              '[SNoOp, SExpr]
              ( ExprTypUnion
                  '[ELit Integer, ELit Double, ELit String, ELit Char]
              )
          )
          (PartType Integer)
      )

--
--AST
--

data AST ce = AST
  { astPackage :: Package
  , astImports :: [Package]
  , astClasses :: [Class ce]
  , astAddons :: [ce]
  } deriving Pretty via (PrettyTree (AST ce))

instance Treeable e => Treeable (AST e) where
  toTree (AST p i c f) = Node
    "AST"
    [ toTree $ "package " ++ showPP p
    , listToTree "imports" i
    , listToTree "classes" c
    , listToTree "funs"    f
    ]

instance Parsable e => Parsable (AST e) where
  parser =
    bracketAny (exactT TBegin) P.eof
      $   AST
      <$> preKw "package" parser
      <*> P.many (preKw "package" parser)
      <*> P.many parser
      <*> P.many parser

instance IsSyntax e => IsSyntax (AST e) where

--
--Class
--

data Class ce = Class
  { className :: ClassName
  , classElems :: [ce]
  } deriving Pretty via (PrettyTree (Class ce))

instance Treeable e => Treeable (Class e) where
  toTree (Class n e) = listToTree ("class " ++ showPP n) e

instance Parsable e => Parsable (Class e) where
  parser = Class <$> (kw "class" *> parser) <*> P.many parser

instance IsSyntax e => IsSyntax (Class e) where

--
--Fun
--

data FunTyp sig stat t = FunTyp
  { funTypName :: Ident
  , funTypSig :: sig t
  , funTypCont :: [stat t]
  }

data FunSigTyp t = FunSigTyp
  { funTypRet :: t
  , funTypArgs :: [(t,Ident)]
  }

instance TypeParsable t => Parsable (Ident,FunSigTyp t) where
  parser = do
    (t, n) <- parserType
    a      <- maybeCommas parserType
    return (n, FunSigTyp t a)

instance (Pretty (t, Ident), Treeable (stat t)) => Treeable (FunTyp FunSigTyp stat t) where
  toTree (FunTyp n (FunSigTyp t as) s) = Node
    ("fun " ++ showPP (t, n))
    (listToTree "args" (showPP <$> as) : toForest s)

deriving via (PrettyTree (FunTyp FunSigTyp stat t))
  instance (Pretty (t, Ident), Treeable (stat t)) => Pretty (FunTyp FunSigTyp stat t)

instance (Parsable (Ident,sig t),Parsable (stat t)) => Parsable (FunTyp sig stat t) where
  parser = uncurry FunTyp <$> parser <*> safeBraces

instance (Pretty (t, Ident), Treeable (stat t), Parsable (Ident,FunSigTyp t), Parsable (stat t)) => IsSyntax (FunTyp FunSigTyp stat t) where

--
--Type
--

data Type = Type Package ClassName

instance Pretty Type where
  showPP (Type p c) = showPP p ++ '.' : classNameString c

data PartType n = FullType Type | NameType ClassName | NoType n

instance (Pretty n) => Pretty (PartType n) where
  showPP (FullType t) = showPP t
  showPP (NameType c) = showPP c
  showPP (NoType   x) = showPP x

instance (Pretty n, Pretty a) => Pretty (PartType n, a) where
  showPP (FullType t, n) = showPP n ++ " : " ++ showPP t
  showPP (NameType c, n) = showPP n ++ " : " ++ showPP c
  showPP (NoType   x, n) = showPP n ++ " : " ++ showPP x

instance TypeParsable (PartType Integer) where
  parserType = do
    a <- optional parser
    i <- parser
    t <- maybe (NoType <$> inc) (return . NameType) a
    return (t, i)
  parserTypeParens = do
    a <- optional (parens parser)
    i <- parser
    t <- maybe (NoType <$> inc) (return . NameType) a
    return (t, i)
  parserNoType = NoType <$> inc

newtype Package = Package [Ident]
  deriving newtype IsList

instance Pretty Package where
  showPP (Package s) = intercalate "." (identString <$> s)

instance Treeable Package where
  toTree = toTree . showPP

instance Parsable Package where
  parser = Package <$> P.sepBy (parser @Ident) (kw ".")

instance IsType (PartType Integer) where

--
--Stat
--

data SNoOp e t = SNoOp
  deriving Pretty via (PrettyTree (SNoOp e t))
instance Parsable (SNoOp e t) where
  parser = kw ";" $> SNoOp
instance Treeable (SNoOp e t) where
  toTree _ = toTree ("no op" :: String)
instance IsSyntax (SNoOp e t) where
instance IsStatTyp SNoOp where

newtype SExpr e t = SExpr (ExprFix e t)
deriving instance (IsExprTyp e, IsType t) => Pretty (SExpr e t)
deriving instance (IsExprTyp e, IsType t) => Treeable (SExpr e t)
deriving instance (IsExprTyp e, IsType t) => Parsable (SExpr e t)
deriving instance (IsExprTyp e, IsType t) => IsSyntax (SExpr e t)
instance IsStatTyp SExpr where

--
--Expr
--

newtype ELit l e t = ELit l
  deriving newtype Parsable
  deriving Pretty via (PrettyTree (ELit l e t))
instance Pretty l => Treeable (ELit l e t) where
  toTree (ELit l) = toTree ("lit " ++ showPP l)
instance (Pretty l, Parsable l) => IsSyntax (ELit l e t) where
instance (Pretty l, Parsable l) => IsExprTyp (ELit l) where
