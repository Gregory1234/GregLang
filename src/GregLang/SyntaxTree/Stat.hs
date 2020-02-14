{-# LANGUAGE DerivingVia, StandaloneDeriving, PolyKinds, OverloadedStrings,
  GeneralizedNewtypeDeriving #-}
module GregLang.SyntaxTree.Stat
  ( module GregLang.SyntaxTree.Stat
  )
where

import           GL.SyntaxTree
import           GL.Parser
import           GL.Utils
import           GL.Ident
import           Text.Megaparsec               as P

sc :: Parser a -> Parser a
sc = (<* optional (kw ";"))

data SNoOp (s :: (* -> *) -> * -> *) e t = SNoOp
  deriving Pretty via (PrettyTree (SNoOp s e t))
instance Parsable (SNoOp s e t) where
  parser = kw ";" $> SNoOp
instance Treeable (SNoOp s e t) where
  toTree _ = toTree ("no op" :: String)
instance IsSyntax (SNoOp s e t)
instance IsStat (SNoOp s)
instance IsStatT SNoOp

newtype SExpr (s :: (* -> *) -> * -> *) e t = SExpr (e t)
deriving instance (Pretty (e t)) => Pretty (SExpr s e t)
deriving instance (Treeable (e t)) => Treeable (SExpr s e t)
instance (Parsable (e t)) => Parsable (SExpr s e t) where
  parser = SExpr <$> sc parser
deriving instance (IsSyntax (e t)) => IsSyntax (SExpr s e t)
instance IsStat (SExpr s)
instance IsStatT SExpr

newtype SBraces s e t = SBraces [s e t]
  deriving Pretty via (PrettyTree (SBraces s e t))
instance Treeable (s e t) => Treeable (SBraces s e t) where
  toTree (SBraces as) = listToTree "braces" as
instance Parsable (s e t) => Parsable (SBraces s e t) where
  parser = SBraces <$> safeBraces
instance IsSyntax (s e t) => IsSyntax (SBraces s e t)
instance IsStat s => IsStat (SBraces s)
instance IsStatT SBraces

data SIf s e t = SIf (e t) (s e t) | SIfElse (e t) (s e t) (s e t)
  deriving Pretty via (PrettyTree (SIf s e t))
instance (Treeable (e t), Treeable (s e t)) => Treeable (SIf s e t) where
  toTree (SIf e s) = Node "if" [toTree e, listToTree "then" [s]]
  toTree (SIfElse e s1 s2) =
    Node "if" [toTree e, listToTree "then" [s1], listToTree "else" [s2]]
instance (Parsable (e t), Parsable (s e t)) => Parsable (SIf s e t) where
  parser = do
    e <- preKw "if" parser
    s <- parser
    (SIfElse e s <$> preKw "else" parser) |> SIf e s
instance (IsSyntax (e t), IsSyntax (s e t)) => IsSyntax (SIf s e t)
instance IsStat s => IsStat (SIf s)
instance IsStatT SIf

data SWhile s e t = SWhile (e t) (s e t) | SDoWhile (s e t) (e t)
  deriving Pretty via (PrettyTree (SWhile s e t))
instance (Treeable (e t), Treeable (s e t)) => Treeable (SWhile s e t) where
  toTree (SWhile   e s) = Node "while" [toTree e, listToTree "do" [s]]
  toTree (SDoWhile s e) = Node "do" [toTree s, listToTree "while" [e]]
instance (Parsable (e t), Parsable (s e t)) => Parsable (SWhile s e t) where
  parser =
    (SWhile <$> preKw "while" parser <*> parser)
      <|> (SDoWhile <$> preKw "do" parser <*> preKw "while" (sc parser))
instance (IsSyntax (e t), IsSyntax (s e t)) => IsSyntax (SWhile s e t)
instance IsStat s => IsStat (SWhile s)
instance IsStatT SWhile

data SFor s e t = SFor (s e t) (e t) (s e t) (s e t)
  deriving Pretty via (PrettyTree (SFor s e t))
instance (Treeable (e t), Treeable (s e t)) => Treeable (SFor s e t) where
  toTree (SFor s1 e s2 s3) =
    Node "for" [toTree s1, toTree e, toTree s2, listToTree "do" [s3]]
instance (Parsable (e t), Parsable (s e t)) => Parsable (SFor s e t) where
  parser = uncurry (uncurry SFor) <$> preKw "for" helper <*> parser
   where
    helper =
      P.try (preKw "(" parser <&> sc parser)
        <&> (parser <* kw ")")
        <|> parser
        <&> sc parser
        <&> parser
instance (IsSyntax (e t), IsSyntax (s e t)) => IsSyntax (SFor s e t)
instance IsStat s => IsStat (SFor s)
instance IsStatT SFor

data SLet s e t = SLet t Ident (e t)
  deriving Pretty via (PrettyTree (SLet s e t))
instance (Treeable (e t), IsType t) => Treeable (SLet s e t) where
  toTree (SLet t n e) = Node ("let " ++ showPP (t, n) ++ " =") [toTree e]
instance (Parsable (e t), IsType t) => Parsable (SLet s e t) where
  parser = sc $ uncurry SLet <$> preKw "let" parserType <*> preKw "=" parser
instance (IsSyntax (e t), IsType t) => IsSyntax (SLet s e t)
instance IsStat (SLet s)
instance IsStatT SLet
