module GL.Data.Token where

import qualified Text.Megaparsec as P(SourcePos)
import Data.Char
import Text.Read
import qualified Text.ParserCombinators.ReadP as RP
import Control.Monad

data Token =
    TBegin
  | TIdent String
  | TStringLit String
  | TIntLit Integer
  | TFloatLit Double
  | TCharLit Char
  | TKeyword Keyword

instance Show Token where
  show TBegin = "<begin>"
  show (TIdent s) = "<ident "++show s++">"
  show (TStringLit s) = "<string "++show s++">"
  show (TIntLit s) = "<int "++show s++">"
  show (TFloatLit s) = "<float "++show s++">"
  show (TCharLit s) = "<char "++show s++">"
  show (TKeyword s) = let x = show s in
    if isAlphaNum (head x)
      then show x
      else "'"++x++"'"

instance Read Token where
  readPrec = foldl1 (<++) [
    TKeyword <$> readPrec,
    TStringLit <$> readPrec,
    TIntLit <$> readPrec,
    TFloatLit <$> readPrec,
    TCharLit <$> readPrec,
    TIdent <$> lift ((:) <$>
      (RP.satisfy (\x -> isAlpha x || x=='_')) <*>
      RP.munch (\x -> isAlphaNum x || x=='_'))]

data Keyword =
    KClass
  | KIf
  | KElse
  | KFor
  | KWhile
  | KLet
  | KBrackOp
  | KBrackCl
  | KBraceOp
  | KBraceCl
  | KParenOp
  | KParenCl deriving (Enum,Bounded)

instance Show Keyword where
  show KClass = "class"
  show KIf = "if"
  show KElse = "else"
  show KFor = "for"
  show KWhile = "while"
  show KLet = "let"
  show KBrackOp = "["
  show KBrackCl = "]"
  show KBraceOp = "{"
  show KBraceCl = "}"
  show KParenOp = "("
  show KParenCl = ")"

instance Read Keyword where
  readPrec = foldl1 (<++) $ map (\x -> lift (RP.string $ show x)*>return x) [minBound..maxBound]

data LocToken = LocToken {
  tokenVal :: Token,
  tokenPos :: P.SourcePos,
  tokenSpellingBefore :: String,
  tokenSpellingDuring :: String
} deriving Show
