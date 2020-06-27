{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module GL.Token.Keyword
  ( module GL.Token.Keyword
  )
where

import           Data.String
import           Data.Char
import           GL.Utils
import           GL.Lexer.Lexable
import           GL.Token.TH
import qualified Data.Text                     as T

keywordType "Operator"
  [("Add","+"),("Sub","-")
  ,("Mul","*"),("Div","/"),("Mod","%")
  ,("And","&&"),("Or","||"),("XOr","^^")
  ,("BAnd","&"),("BOr","|"),("BXOr","^")]

instance LexerState s => Lexable s Operator where
  consume = enumToken fromOperator

instance IsString Operator where
  fromString = toOperator . T.pack

keywordType "Comparasion"
  [("Eq","=="),("NEq","!=")
  ,("GEq",">="),("LEq","<=")
  ,("Gt",">"),("Lt","<")]

instance LexerState s => Lexable s Comparasion where
  consume = enumToken fromComparasion

instance IsString Comparasion where
  fromString = toComparasion . T.pack

keywordType "OtherSymbol"
  [("Not","!"),("BNot","~")
  ,("Inc","++"),("Dec","--")
  ,("QMark","?"),("Colon",":")
  ,("Semicolon",";"),("Comma",","),("Dot",".")]

instance LexerState s => Lexable s OtherSymbol where
  consume = enumToken fromOtherSymbol

instance IsString OtherSymbol where
  fromString = toOtherSymbol . T.pack

keywordType "Keyword"
  (map (\a -> (a,mapMaybe (liftA2 toMaybe isAlpha toLower) a))
    ["If","For","While","Do","Switch"
    ,"Break","Continue","Return"
    ,"Let","This","True'","False'"
    ,"Package","Import","Class"])

instance LexerState s => Lexable s Keyword where
  consume = enumToken fromKeyword

instance IsString Keyword where
  fromString = toKeyword . T.pack

keywordType "BracketType" [("Bracks","[]"),("Parens","()"),("Braces","{}")]

data BracketState = OpenB | ClosedB
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Bracket = (BracketType, BracketState)

fromBracket :: Bracket -> Text
fromBracket (b, OpenB  ) = T.singleton (fromBracketType b `T.index` 0)
fromBracket (b, ClosedB) = T.singleton (fromBracketType b `T.index` 1)

instance LexerState s => Lexable s Bracket where
  consume =
    asum $ fmap (\x -> string (fromBracket x) $> x) (enumerate <&> enumerate)

data Symbol
  = OpSym Operator
  | OtherSym OtherSymbol
  | SetOpSym (Maybe Operator)
  | CompOpSym Comparasion
  | BrSym Bracket
  deriving (Eq, Ord, Show, Read)

instance LexerState s => Lexable s Symbol where
  consume = asum
    [ CompOpSym <$> consume
    , SetOpSym . Just <$> (consume <* string "=")
    , SetOpSym <$> (string "=" $> Nothing)
    , OpSym <$> consume
    , BrSym <$> consume
    ]

instance IsString Symbol where
  fromString = fromJust . evalLexerS consume . EmptyState . T.pack

fromSymbol :: Symbol -> Text
fromSymbol (OpSym     x       ) = fromOperator x
fromSymbol (OtherSym  x       ) = fromOtherSymbol x
fromSymbol (SetOpSym  (Just x)) = fromOperator x <> "="
fromSymbol (SetOpSym  Nothing ) = "="
fromSymbol (CompOpSym x       ) = fromComparasion x
fromSymbol (BrSym     x       ) = fromBracket x
