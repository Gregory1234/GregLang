module GL.Token.Keyword
  ( module GL.Token.Keyword
  )
where

import           GL.Utils
import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read               hiding ( Ident )
import           Data.String

data Operator = Add | Sub | Mul | Div | And | Mod | Or | XOr | BAnd | BOr | BXor
  deriving (Eq, Ord)

instance Lexable Operator where
  lexAP = lift $ RP.choice []

data Comparasion = Eq | NEq | Gt | Lt | GEq | LEq
  deriving (Eq, Ord)

instance Lexable Comparasion where
  lexAP = lift $ RP.choice []

data OtherOperator = Not | Inc | Dec
  deriving (Eq, Ord)

instance Lexable OtherOperator where
  lexAP = lift $ RP.choice []

data ControlStructure = If | For | While | Do | Switch | Break | Continue
  deriving (Eq, Ord)

instance Lexable ControlStructure where
  lexAP = lift $ RP.choice []

data BracketType = Bracks | Parens | Braces
  deriving (Eq, Ord)

instance Lexable BracketType where
  lexAP = lift $ RP.choice []

data GlobalKeyword = Package | Import | Class
  deriving (Eq, Ord)

instance Lexable GlobalKeyword where
  lexAP = lift $ RP.choice []

data Keyword
  = OKeyword Operator
  | OOKeyword OtherOperator
  | OSKeyword Operator
  | CKeyword Comparasion
  | CSKeyword ControlStructure
  | BKeyword BracketType Bool
  | GKeyword GlobalKeyword
  deriving (Eq, Ord)

instance Lexable Keyword where
  lexAP = lift $ RP.choice []

instance IsString Keyword where
  fromString _ = undefined

getKeyword :: Keyword -> String
getKeyword _ = "TODO"
