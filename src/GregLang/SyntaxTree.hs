{-# LANGUAGE DerivingVia, StandaloneDeriving, DataKinds, FlexibleInstances,
  FlexibleContexts, UndecidableInstances, OverloadedStrings #-}
module GregLang.SyntaxTree
  ( module GregLang.SyntaxTree
  , module GregLang.SyntaxTree.Expr
  , module GregLang.SyntaxTree.Stat
  , module GregLang.SyntaxTree.Type
  )
where

import           GregLang.SyntaxTree.Expr
import           GregLang.SyntaxTree.Stat
import           GregLang.SyntaxTree.Type
import           GL.Ident
import           GL.Utils
import           GL.Parser
import           GL.SyntaxTree
import           GL.Type
import           GL.Token
import           Text.Megaparsec               as P

type SetOps
  = '[('("+", "add")), '("-", "sub"), '("*", "mul"), '("/", "div"), '("%", "mod"), '("&", "and"), '("|", "or"), '("^", "xor"), '("&&", "bin and"), '("||", "bin or"), '("^^", "bin xor")]
type Statements = '[SIf, SWhile, SFor, SLet, SBraces, SNoOp, SSet SetOps, SExpr]
type Expressions = '[ELit Integer, ELit Double, ELit String, ELit Char, EVar]
type ExpressionsT
  = '[EIf, EBOr, EBXor, EBAnd, EOr, EXor, EAnd, EEq, EComp, EAdd, EMul, EPre, EDot, EParens]

type DefaultExpr = ExprTDo ExpressionsT (ExprTyped (ExprUnion Expressions))
type DefaultStat = StatTUnion Statements DefaultExpr
type UntypedAST = AST (FunTyp FunSigTyp DefaultStat (PartType Integer))

--
--AST
--

data AST ce = AST
  { astPackage :: Package
  , astImports :: [Package]
  , astClasses :: [Class ce]
  , astAddons :: [ce]
  }

instance Treeable e => Treeable (AST e) where
  toTree (AST p i c f) = Node
    "AST"
    [ toTree $ "package " ++ treePP p
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
  }

instance Treeable e => Treeable (Class e) where
  toTree (Class n e) = listToTree ("class " ++ treePP n) e

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

instance IsType t => Parsable (Ident,FunSigTyp t) where
  parser = do
    (t, n) <- parserType
    a      <- maybeCommas parserType
    return (n, FunSigTyp t a)

instance (IsType t, Treeable (stat t))
  => Treeable (FunTyp FunSigTyp stat t) where
  toTree (FunTyp n (FunSigTyp t as) s) = Node
    ("fun " ++ typeAnnotate t (getIdent n))
    ( listToTree "args" (uncurry typeAnnotate . (fmap getIdent) <$> as)
    : toForest s
    )

instance (Parsable (Ident,sig t),Parsable (stat t))
  => Parsable (FunTyp sig stat t) where
  parser = uncurry FunTyp <$> parser <*> safeBraces

instance (IsType t, Treeable (stat t), Parsable (Ident,FunSigTyp t), Parsable (stat t))
  => IsSyntax (FunTyp FunSigTyp stat t) where
