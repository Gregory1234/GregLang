{-# LANGUAGE TupleSections, FlexibleInstances #-}
module GL.Token.Keyword
  ( module GL.Token.Keyword
  )
where

import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read               hiding ( Ident )
import           Data.String
import           Data.Functor
import           Data.Char
import           Data.Bool
import           GL.Utils
import           GL.Lexer.Lexable



funToLexable :: (Enum a, Bounded a) => (a -> String) -> ReadPrec a
funToLexable f =
  lift $ foldr1 (RP.<++) (map (\x -> RP.string (f x) $> x) enumerate)

data Operator = Add | Sub | Mul | Div | Mod | And | Or | XOr | BAnd | BOr | BXOr
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getOperator :: Operator -> String
getOperator Add  = "+"
getOperator Sub  = "-"
getOperator Mul  = "*"
getOperator Div  = "/"
getOperator Mod  = "%"
getOperator And  = "&&"
getOperator Or   = "||"
getOperator XOr  = "^^"
getOperator BAnd = "&"
getOperator BOr  = "|"
getOperator BXOr = "^"

instance Lexable Operator where
  lexAP = funToLexable getOperator

instance IsString Operator where
  fromString = lexS

data Comparasion = Eq | NEq | GEq | LEq | Gt | Lt
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getComparasion :: Comparasion -> String
getComparasion Eq  = "=="
getComparasion NEq = "!="
getComparasion Gt  = ">"
getComparasion Lt  = "<"
getComparasion GEq = ">="
getComparasion LEq = "<="

instance Lexable Comparasion where
  lexAP = funToLexable getComparasion

instance IsString Comparasion where
  fromString = lexS

data OtherSymbol
  = Not | BNot | Inc | Dec | QMark | Colon | Semicolon | Comma | Dot
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getOtherOperator :: OtherSymbol -> String
getOtherOperator Not       = "!"
getOtherOperator BNot      = "~"
getOtherOperator Inc       = "++"
getOtherOperator Dec       = "--"
getOtherOperator QMark     = "?"
getOtherOperator Colon     = ":"
getOtherOperator Semicolon = ";"
getOtherOperator Comma     = ","
getOtherOperator Dot       = "."

instance Lexable OtherSymbol where
  lexAP = funToLexable getOtherOperator

instance IsString OtherSymbol where
  fromString = lexS

data ReservedKeyword
  = If | For | While | Do | Switch
  | Break | Continue
  | Let | This
  | Package | Import | Class
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getReservedKeyword :: ReservedKeyword -> String
getReservedKeyword = map toLower . show

instance Lexable ReservedKeyword where
  lexAP = funToLexable getReservedKeyword

instance IsString ReservedKeyword where
  fromString = lexS

data BracketType = Bracks | Parens | Braces
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getBracket :: BracketType -> (String, String)
getBracket Bracks = ("[", "]")
getBracket Parens = ("(", ")")
getBracket Braces = ("{", "}")

type Bracket = (BracketType, Bool)

instance Lexable Bracket where
  lexAP = helper False fst <|> helper True snd
    where helper b f = (, b) <$> funToLexable (f . getBracket)

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

getKeyword :: Keyword -> String
getKeyword (OKeyword  x       ) = getOperator x
getKeyword (SKeyword  x       ) = getOtherOperator x
getKeyword (OSKeyword (Just x)) = getOperator x ++ "="
getKeyword (OSKeyword Nothing ) = "="
getKeyword (CKeyword  x       ) = getComparasion x
getKeyword (RKeyword  x       ) = getReservedKeyword x
getKeyword (BKeyword  (x, b)  ) = bool fst snd b $ getBracket x
