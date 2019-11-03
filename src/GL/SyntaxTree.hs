{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingVia, UndecidableInstances,
  FlexibleInstances, StandaloneDeriving #-}

module GL.SyntaxTree
  ( module GL.SyntaxTree
  )
where

import           GHC.Exts
import           Data.List
import           GL.Ident
import           GL.Utils

data AST ce = AST
  { astPackage :: Package
  , astImports :: [Package]
  , astClasses :: [Class ce]
  , astAddons :: [ce]
  } deriving Pretty via (PrettyTree (AST ce))

instance Treeable ce => Treeable (AST ce) where
  toTree (AST p i c f) = Node
    "AST"
    [ toTree $ "package " ++ showPP p
    , listToTree "imports" i
    , listToTree "classes" c
    , listToTree "funs"    f
    ]

type UntypedAST
  = AST
      ( Fun
          (FunSigTyp (PartType Integer))
          (StatTyp (PartType Integer) (ExprTyp Expr))
      )

data Class ce = Class
  { className :: ClassName
  , classElems :: [ce]
  } deriving Pretty via (PrettyTree (Class ce))

instance Treeable ce => Treeable (Class ce) where
  toTree (Class n e) = listToTree ("class " ++ showPP n) e

data Fun sig cont = Fun
  { funName :: Ident
  , funSig :: sig
  , funCont :: cont
  }

data FunSigTyp t = FunSigTyp
  { funRet :: t
  , funArgs :: [(t,Ident)]
  }

instance (Pretty (t, Ident), Treeable cont) => Treeable (Fun (FunSigTyp t) cont) where
  toTree (Fun n (FunSigTyp t as) s) =
    let (Node _ c) = toTree s
    in  Node (" fun " ++ showPP (t, n)) (listToTree "args" (showPP <$> as) : c)

deriving via (PrettyTree (Fun (FunSigTyp t) cont))
  instance (Pretty (t, Ident), Treeable cont) => Pretty (Fun (FunSigTyp t) cont)

data Type = Type Package ClassName

instance Pretty Type where
  showPP (Type p c) = showPP p ++ '.' : classNameString c

data PartType n = FullType Type | NameType ClassName | NoType n

instance Pretty n => Pretty (PartType n, Ident) where
  showPP (FullType t, n) = identString n ++ " : " ++ showPP t
  showPP (NameType c, n) = identString n ++ " : " ++ showPP c
  showPP (NoType   x, n) = identString n ++ " : " ++ showPP x

newtype Package = Package [Ident]
  deriving newtype IsList

instance Pretty Package where
  showPP (Package s) = intercalate "." (identString <$> s)

instance Treeable Package where
  toTree = toTree . showPP

data StatTyp t e =
    SIf (e t) (StatTyp t e) (Maybe (StatTyp t e))
  | SFor (StatTyp t e) (e t) (StatTyp t e) (StatTyp t e)
  | SWhile (e t) (StatTyp t e)
  | SDoWhile (e t) (StatTyp t e)
  | SLet t Ident (e t)
  | SReturn (e t)
  | SBreak
  | SContinue
  | SNoOp
  | SBraces [StatTyp t e]
  | SExpr (e t)

deriving via (PrettyTree (StatTyp t e))
  instance (Pretty (t, Ident), Treeable (e t)) => Pretty (StatTyp t e)

instance (Pretty (t, Ident), Treeable (e t)) => Treeable (StatTyp t e) where
  toTree (SIf e s Nothing) = Node "if" [toTree e, Node "then" [toTree s]]
  toTree (SIf e s1 (Just s2)) =
    Node "if" [toTree e, Node "then" [toTree s1], Node "else" [toTree s2]]
  toTree (SFor s1 e s2 s3) =
    Node "for" [toTree s1, toTree e, toTree s2, Node "do" [toTree s3]]
  toTree (SWhile   e s) = Node "while" [toTree e, Node "do" [toTree s]]
  toTree (SDoWhile e s) = Node "do" [toTree s, Node "while" [toTree e]]
  toTree (SLet t n e  ) = Node ("let " ++ showPP (t, n) ++ " =") [toTree e]
  toTree (SReturn e   ) = Node "return" [toTree e]
  toTree SBreak         = toTree "break"
  toTree SContinue      = toTree "continue"
  toTree SNoOp          = toTree "no op"
  toTree (SBraces s)    = listToTree "braces" s
  toTree (SExpr   e)    = toTree e

data ExprTyp e t = ExprTyp t e

deriving via (PrettyTree (ExprTyp e t))
  instance (Pretty (t,Ident), Treeable e) => Pretty (ExprTyp e t)

instance (Pretty (t,Ident), Treeable e) => Treeable (ExprTyp e t) where
  toTree (ExprTyp t e) =
    let (Node a b) = toTree e in Node (showPP (t, Ident a)) b

data Expr =
    EIntLit Integer
  | EFloatLit Double
  | ECharLit Char
  | EStringLit String
  | EVar (Maybe Expr) Ident [Expr]
  | EIf Expr Expr Expr
  | EParen Expr
    deriving Pretty via (PrettyTree Expr)

instance Treeable Expr where
  toTree (EIntLit    i      ) = toTree $ showPP i
  toTree (EFloatLit  f      ) = toTree $ showPP f
  toTree (EStringLit s      ) = toTree $ showPP s
  toTree (ECharLit   c      ) = toTree $ showPP c
  toTree (EVar Nothing  n []) = toTree n
  toTree (EVar (Just d) n []) = Node (showPP n) [Node "of" [toTree d]]
  toTree (EVar Nothing  n xs) = Node (showPP n) [listToTree "args" xs]
  toTree (EVar (Just d) n xs) =
    Node (showPP n) [Node "of" [toTree d], listToTree "args" xs]
  toTree (EIf e1 e2 e3) =
    Node "if" [toTree e1, Node "then" [toTree e2], Node "else" [toTree e3]]
  toTree (EParen e) = Node "parens" [toTree e]
