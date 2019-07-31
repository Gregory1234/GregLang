module Main where

import Data.Char
import GL.Data.Token
import GL.Lexer
import Test.QuickCheck
import qualified Text.Megaparsec as P

instance Arbitrary Keyword where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Token where
  arbitrary =
    oneof
      [ TIdent <$>
        ((:) <$> (arbitrary `suchThat` (\c -> isAlpha c || c == '_')) <*>
         listOf (arbitrary `suchThat` (\c -> isAlphaNum c || c == '_')))
      , TIntLit <$> (arbitrary `suchThat` (>= 0))
      , TKeyword <$> arbitrary
      ]

{-instance Arbitrary LocToken where
  arbitrary = do
    tok <- arbitrary
    pos <- arbitrary
    sd <- mapM (\x -> elements [toUpper x, toLower x]) (spellToken tok)
    sa <- listOf (arbitrary `suchThat` isSpace)
    return $ LocToken tok pos sd sa-}
newtype TokenStream =
  TokenStream [LocToken]
  deriving (Show)

{-arbitrarySpelling (TKeyword a) =
  mapM (\x -> elements [toUpper x, toLower x]) (show a)-}
arbitrarySpelling x = return $ spellToken x

arbitraryTokenStream :: Int -> P.SourcePos -> Gen [LocToken]
arbitraryTokenStream 0 _ = return []
arbitraryTokenStream n p = do
  tok <- arbitrary
  sd <- arbitrarySpelling tok
  sa <- listOf1 (arbitrary `suchThat` isSpace)
  (LocToken tok p sd sa :) <$>
    arbitraryTokenStream (n - 1) (updatePosString p (sd ++ sa))

instance Arbitrary TokenStream where
  arbitrary = do
    n <- getSize
    sf <- listOf (arbitrary `suchThat` isSpace)
    TokenStream . (LocToken TBegin (P.initialPos "") "" sf :) <$>
      arbitraryTokenStream n (updatePosString (P.initialPos "") sf)

lexerReversable :: TokenStream -> Property
lexerReversable (TokenStream l) =
  Right l === lexGregLang "" (l >>= recreateToken)

main = quickCheck lexerReversable
