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

instance Lexable Operator where
  consume = enumToken fromOperator

instance IsString Operator where
  fromString = toOperator . T.pack

keywordType "Comparasion"
  [("Eq","=="),("NEq","!=")
  ,("GEq",">="),("LEq","<=")
  ,("Gt",">"),("Lt","<")]

instance Lexable Comparasion where
  consume = enumToken fromComparasion

instance IsString Comparasion where
  fromString = toComparasion . T.pack

keywordType "OtherSymbol"
  [("Not","!"),("BNot","~")
  ,("Inc","++"),("Dec","--")
  ,("QMark","?"),("Colon",":")
  ,("Semicolon",";"),("Comma",","),("Dot",".")]

instance Lexable OtherSymbol where
  consume = enumToken fromOtherSymbol

instance IsString OtherSymbol where
  fromString = toOtherSymbol . T.pack

keywordType "ReservedKeyword"
  (map (\a -> (a,mapMaybe (liftA2 toMaybe isAlpha toLower) a))
    ["If","For","While","Do","Switch"
    ,"Break","Continue","Return"
    ,"Let","This","True'","False'"
    ,"Package","Import","Class"])

instance Lexable ReservedKeyword where
  consume = enumToken fromReservedKeyword

instance IsString ReservedKeyword where
  fromString = toReservedKeyword . T.pack

keywordType "BracketType" [("Bracks","[]"),("Parens","()"),("Braces","{}")]

data BracketState = OpenB | ClosedB
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Bracket = (BracketType, BracketState)

fromBracket :: Bracket -> Text
fromBracket (b, OpenB  ) = T.singleton (fromBracketType b `T.index` 0)
fromBracket (b, ClosedB) = T.singleton (fromBracketType b `T.index` 1)

instance Lexable Bracket where
  consume =
    asum $ fmap (\x -> string (fromBracket x) $> x) (enumerate <&> enumerate)

data Keyword
  = OKeyword Operator
  | SKeyword OtherSymbol
  | OSKeyword (Maybe Operator)
  | CKeyword Comparasion
  | RKeyword ReservedKeyword
  | BKeyword Bracket
  deriving (Eq, Ord, Show, Read)

instance Lexable Keyword where
  consume = asum
    [ CKeyword <$> consume
    , OSKeyword . Just <$> (consume <* string "=")
    , OSKeyword <$> (string "=" $> Nothing)
    , SKeyword <$> consume
    , OKeyword <$> consume
    , BKeyword <$> consume
    , RKeyword <$> consume
    ]

instance IsString Keyword where
  fromString = fromJust . evalLexer consume "" . T.pack

fromKeyword :: Keyword -> Text
fromKeyword (OKeyword  x       ) = fromOperator x
fromKeyword (SKeyword  x       ) = fromOtherSymbol x
fromKeyword (OSKeyword (Just x)) = fromOperator x <> "="
fromKeyword (OSKeyword Nothing ) = "="
fromKeyword (CKeyword  x       ) = fromComparasion x
fromKeyword (RKeyword  x       ) = fromReservedKeyword x
fromKeyword (BKeyword  x       ) = fromBracket x
