{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module GL.Lexer.Lexable
  ( module GL.Lexer.Lexable
  )
where

import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read               hiding ( Lexeme(..) )
import           Data.Char
import           GL.Utils

-- Parsing of 'String's, producing values.
class Lexable a where
  -- | 'readPrec' for 'Lexable'
  lexAP :: ReadPrec a
  default lexAP :: Read a => ReadPrec a
  lexAP = readPrec

-- | 'reads' for 'Lexable'
lexA :: Lexable a => String -> [(a, String)]
lexA = readPrec_to_S lexAP 0

-- | 'read' for 'Lexable'
lexS :: Lexable a => String -> a
lexS = fst . head . lexA

instance Lexable Int
instance Lexable Integer
instance Lexable Float
instance Lexable Double
instance Lexable String
instance Lexable Char

-- | Read both the value and the used up 'String'.
lexGather :: Lexable a => ReadS (String, a)
lexGather = RP.readP_to_S (RP.gather (readPrec_to_P lexAP 0))


instance Lexable ClassName where
  lexAP = ClassName <$> lift
    ((:) <$> RP.satisfy (isAlpha &&& isUpper) <*> RP.munch
      (isAlphaNum ||| (== '_'))
    )
instance Lexable Ident where
  lexAP = Ident <$> lift
    ((:) <$> RP.satisfy (isAlpha &&& isLower ||| (== '_')) <*> RP.munch
      (isAlphaNum ||| (== '_'))
    )
