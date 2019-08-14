{-# LANGUAGE DerivingVia #-}
module GL.Data.Ident where

import           Data.Char
import           GL.Utils
import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read               hiding ( Ident )
import           Data.String

newtype Ident =
  Ident { identString :: String }
  deriving stock (Eq,Ord)
  deriving Treeable via String
  deriving Show via ClearShow
  deriving IsString via String

instance Read Ident where
  readPrec = Ident <$> lift
    ((:) <$> RP.satisfy (isAlpha &&& isLower ||| (== '_')) <*> RP.munch
      (isAlphaNum ||| (== '_'))
    )

newtype ClassName =
  ClassName { classNameString :: String }
  deriving stock (Eq,Ord)
  deriving Treeable via String
  deriving Show via ClearShow
  deriving IsString via String


instance Read ClassName where
  readPrec = ClassName <$> lift
    ((:) <$> RP.satisfy (isAlpha &&& isUpper) <*> RP.munch
      (isAlphaNum ||| (== '_'))
    )
