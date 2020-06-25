{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module GL.Token.Keyword
  ( module GL.Token.Keyword
  )
where

import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read               hiding ( Ident )
import           Data.String
import           Data.Functor
import           Data.Char
import           GL.Utils
import           GL.Lexer.Lexable
import           GL.Token.TH

funToLexable :: (Enum a, Bounded a) => (a -> String) -> ReadPrec a
funToLexable f =
  lift $ foldr1 (RP.<++) (map (\x -> RP.string (f x) $> x) enumerate)

keywordType "Operator"
  [("Add","+"),("Sub","-")
  ,("Mul","*"),("Div","/"),("Mod","%")
  ,("And","&&"),("Or","||"),("XOr","^^")
  ,("BAnd","&"),("BOr","|"),("BXOr","^")]

instance Lexable Operator where
  lexAP = funToLexable fromOperator

instance IsString Operator where
  fromString = toOperator

keywordType "Comparasion"
  [("Eq","=="),("NEq","!=")
  ,("GEq",">="),("LEq","<=")
  ,("Gt",">"),("Lt","<")]

instance Lexable Comparasion where
  lexAP = funToLexable fromComparasion

instance IsString Comparasion where
  fromString = toComparasion

keywordType "OtherSymbol"
  [("Not","!"),("BNot","~")
  ,("Inc","++"),("Dec","--")
  ,("QMark","?"),("Colon",":")
  ,("Semicolon",";"),("Comma",","),("Dot",".")]

instance Lexable OtherSymbol where
  lexAP = funToLexable fromOtherSymbol

instance IsString OtherSymbol where
  fromString = toOtherSymbol

keywordType "ReservedKeyword"
  (map (\a -> (a,mapMaybe (liftA2 toMaybe isAlpha toLower) a))
    ["If","For","While","Do","Switch"
    ,"Break","Continue","Return"
    ,"Let","This","True'","False'"
    ,"Package","Import","Class"])

instance Lexable ReservedKeyword where
  lexAP = funToLexable fromReservedKeyword

instance IsString ReservedKeyword where
  fromString = toReservedKeyword

keywordType "BracketType" [("Bracks","[]"),("Parens","()"),("Braces","{}")]

data BracketState = OpenB | ClosedB
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Bracket = (BracketType, BracketState)

fromBracket :: Bracket -> String
fromBracket (b, OpenB  ) = let [o, _] = fromBracketType b in [o]
fromBracket (b, ClosedB) = let [_, c] = fromBracketType b in [c]

instance Lexable Bracket where
  lexAP = helper OpenB <|> helper ClosedB
    where helper b = (, b) <$> funToLexable (fromBracket . (, b))

instance IsString Bracket where
  fromString = lexS

data Keyword
  = OKeyword Operator
  | SKeyword OtherSymbol
  | OSKeyword (Maybe Operator)
  | CKeyword Comparasion
  | RKeyword ReservedKeyword
  | BKeyword Bracket
  deriving (Eq, Ord, Show, Read)

instance Lexable Keyword where
  lexAP = foldr1
    (<++)
    [ CKeyword <$> lexAP
    , OSKeyword . Just <$> (lexAP <* lift (RP.char '='))
    , OSKeyword <$> (lift (RP.char '=') $> Nothing)
    , SKeyword <$> lexAP
    , OKeyword <$> lexAP
    , BKeyword <$> lexAP
    , RKeyword <$> lexAP
    ]

instance IsString Keyword where
  fromString = lexS

fromKeyword :: Keyword -> String
fromKeyword (OKeyword  x       ) = fromOperator x
fromKeyword (SKeyword  x       ) = fromOtherSymbol x
fromKeyword (OSKeyword (Just x)) = fromOperator x ++ "="
fromKeyword (OSKeyword Nothing ) = "="
fromKeyword (CKeyword  x       ) = fromComparasion x
fromKeyword (RKeyword  x       ) = fromReservedKeyword x
fromKeyword (BKeyword  x       ) = fromBracket x
