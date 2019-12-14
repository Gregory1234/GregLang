{-# LANGUAGE DerivingVia, StandaloneDeriving, PolyKinds, OverloadedStrings,
  GeneralizedNewtypeDeriving #-}
module GregLang.SyntaxTree.Stat
  ( module GregLang.SyntaxTree.Stat
  )
where

import           GL.SyntaxTree
import           GL.Parser
import           GL.Utils
import           Text.Megaparsec               as P

sc :: Parser a -> Parser a
sc = (<* optional (kw ";"))

data SNoOp e t = SNoOp
  deriving Pretty via (PrettyTree (SNoOp e t))
instance Parsable (SNoOp e t) where
  parser = kw ";" $> SNoOp
instance Treeable (SNoOp e t) where
  toTree _ = toTree ("no op" :: String)
instance IsSyntax (SNoOp e t)

newtype SExpr e t = SExpr (e t)
deriving instance (Pretty (e t)) => Pretty (SExpr e t)
deriving instance (Treeable (e t)) => Treeable (SExpr e t)
instance (Parsable (e t)) => Parsable (SExpr e t) where
  parser = SExpr <$> sc parser
deriving instance (IsSyntax (e t)) => IsSyntax (SExpr e t)

newtype SBraces s e t = SBraces [s]
  deriving Pretty via (PrettyTree (SBraces s e t))
instance Treeable s => Treeable (SBraces s e t) where
  toTree (SBraces as) = listToTree "braces" as
instance Parsable s => Parsable (SBraces s e t) where
  parser = SBraces <$> safeBraces
instance IsSyntax s => IsSyntax (SBraces s e t)

data SIf s e t = SIf (e t) s | SIfElse (e t) s s
  deriving Pretty via (PrettyTree (SIf s e t))
instance (Treeable (e t), Treeable s) => Treeable (SIf s e t) where
  toTree (SIf e s) = Node "if" [toTree e, listToTree "then" [s]]
  toTree (SIfElse e s1 s2) =
    Node "if" [toTree e, listToTree "then" [s1], listToTree "else" [s2]]
instance (Parsable (e t), Parsable s) => Parsable (SIf s e t) where
  parser = do
    e <- preKw "if" parser
    s <- parser
    (SIfElse e s <$> preKw "else" parser) |> SIf e s
instance (IsSyntax (e t), IsSyntax s) => IsSyntax (SIf s e t)


data SWhile s e t = SWhile (e t) s | SDoWhile s (e t)
  deriving Pretty via (PrettyTree (SWhile s e t))
instance (Treeable (e t), Treeable s) => Treeable (SWhile s e t) where
  toTree (SWhile   e s) = Node "while" [toTree e, listToTree "do" [s]]
  toTree (SDoWhile s e) = Node "do" [toTree s, listToTree "while" [e]]
instance (Parsable (e t), Parsable s) => Parsable (SWhile s e t) where
  parser =
    (SWhile <$> preKw "while" parser <*> parser)
      <|> (SDoWhile <$> preKw "do" parser <*> preKw "while" (sc parser))
instance (IsSyntax (e t), IsSyntax s) => IsSyntax (SWhile s e t)

data SFor s e t = SFor s (e t) s s
  deriving Pretty via (PrettyTree (SFor s e t))
instance (Treeable (e t), Treeable s) => Treeable (SFor s e t) where
  toTree (SFor s1 e s2 s3) =
    Node "for" [toTree s1, toTree e, toTree s2, listToTree "do" [s3]]
instance (Parsable (e t), Parsable s) => Parsable (SFor s e t) where
  parser = uncurry (uncurry SFor) <$> preKw "for" helper <*> parser
   where
    helper =
      P.try (preKw "(" parser <&> sc parser)
        <&> (parser <* kw ")")
        <|> parser
        <&> sc parser
        <&> parser
instance (IsSyntax (e t), IsSyntax s) => IsSyntax (SFor s e t)
