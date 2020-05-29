{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving #-}
module GL.Ident
  ( module GL.Ident
  )
where

import           Data.Char
import           GL.Utils
import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read               hiding ( Ident )
import           Data.String
import           GL.Lexer.Lexable

newtype Ident =
  Ident { getIdent :: String }
  deriving newtype (Eq, Ord, IsString, Treeable, Show)

instance Lexable Ident where
  lexAP = Ident <$> lift
    ((:) <$> RP.satisfy (isAlpha &&& isLower ||| (== '_')) <*> RP.munch
      (isAlphaNum ||| (== '_'))
    )

newtype ClassName =
  ClassName { getClassName :: String }
  deriving newtype (Eq, Ord, IsString, Treeable, Show)


instance Lexable ClassName where
  lexAP = ClassName <$> lift
    ((:) <$> RP.satisfy (isAlpha &&& isUpper) <*> RP.munch
      (isAlphaNum ||| (== '_'))
    )
