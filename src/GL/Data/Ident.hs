{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving #-}
module GL.Data.Ident
  ( Ident(..)
  , ClassName(..)
  )
where

import           Data.Char
import           GL.Utils
import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read               hiding ( Ident )
import           Data.String

newtype Ident =
  Ident { identString :: String }
  deriving newtype (Eq, Ord, IsString, Treeable)
  deriving Show via ClearShow

instance Read Ident where
  readPrec = Ident <$> lift
    ((:) <$> RP.satisfy (isAlpha &&& isLower ||| (== '_')) <*> RP.munch
      (isAlphaNum ||| (== '_'))
    )

newtype ClassName =
  ClassName { classNameString :: String }
  deriving newtype (Eq, Ord, IsString, Treeable)
  deriving Show via ClearShow


instance Read ClassName where
  readPrec = ClassName <$> lift
    ((:) <$> RP.satisfy (isAlpha &&& isUpper) <*> RP.munch
      (isAlphaNum ||| (== '_'))
    )
