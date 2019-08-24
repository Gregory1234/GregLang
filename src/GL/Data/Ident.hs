{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving #-}
module GL.Data.Ident
  ( module GL.Data.Ident
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
  deriving Pretty via ClearString

instance Lexable Ident where
  lexAP = Ident <$> lift
    ((:) <$> RP.satisfy (isAlpha &&& isLower ||| (== '_')) <*> RP.munch
      (isAlphaNum ||| (== '_'))
    )

newtype ClassName =
  ClassName { classNameString :: String }
  deriving newtype (Eq, Ord, IsString, Treeable)
  deriving Pretty via ClearString


instance Lexable ClassName where
  lexAP = ClassName <$> lift
    ((:) <$> RP.satisfy (isAlpha &&& isUpper) <*> RP.munch
      (isAlphaNum ||| (== '_'))
    )
