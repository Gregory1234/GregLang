module Main where

import Data.Char
import GL.Data.Token
import GL.Lexer
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck
import qualified Text.Megaparsec as P

instance Arbitrary Keyword where
  arbitrary = elements [minBound .. maxBound]
  shrink KIf = []
  shrink x = [KIf]

instance Arbitrary Token where
  arbitrary =
    oneof
      [ TIdent <$>
        (((:) <$> (arbitrary `suchThat` (\c -> isAlpha c || c == '_')) <*>
          listOf (arbitrary `suchThat` (\c -> isAlphaNum c || c == '_'))) `suchThat`
         (`notElem` map show keywords))
      , TIntLit <$> (arbitrary `suchThat` (>= 0))
      , TKeyword <$> arbitrary
      ]
  shrink TBegin = []
  shrink (TIdent x) =
    TIdent <$> filter (`notElem` map show keywords) (shrinkIdent x)
    where
      shrinkIdent [x] = pure <$> shrinkChar x
      shrinkIdent (x:xs) =
        (x : init xs) :
        xs : (pure <$> shrinkChar x) ++ ((x :) <$> shrinkIdent xs)
      shrinkChar x
        | x == 'a' = []
        | x == 'A' = ['a']
        | x == '1' = ['a']
        | isUpper x = ['A']
        | isLower x = ['a']
        | isDigit x = ['1']
        | otherwise = ['a']
  shrink (TIntLit x) = TIntLit <$> filter (>= 0) (shrink x)
  shrink (TKeyword x) = TKeyword <$> shrink x

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

arbitraryTokenStream :: Int -> Gen [LocToken]
arbitraryTokenStream 0 = return []
arbitraryTokenStream n = do
  tok <- arbitrary
  sd <- arbitrarySpelling tok
  sa <- listOf1 (arbitrary `suchThat` isSpace)
  (LocToken tok (P.initialPos "") sd sa :) <$> arbitraryTokenStream (n - 1)

instance Arbitrary TokenStream where
  arbitrary = do
    n <- getSize
    sf <- listOf (arbitrary `suchThat` isSpace)
    TokenStream . (LocToken TBegin (P.initialPos "") "" sf :) <$>
      arbitraryTokenStream n
  shrink (TokenStream (b:l)) = TokenStream . (b :) <$> shrinkTokenStream l
    where
      shrinkTokenStream [] = []
      shrinkTokenStream [x] = pure <$> shrinkLocToken x
      shrinkTokenStream (x:xs) =
        (x : init xs) :
        xs : (pure <$> shrinkLocToken x) ++ ((x :) <$> shrinkTokenStream xs)
      shrinkLocToken (LocToken t p s1 s2) =
        (\x -> LocToken x p (spellToken x) s2) <$> shrink t

lexerReversable :: TokenStream -> Property
lexerReversable (TokenStream l) =
  Right (tokenVal <$> l) ===
  (map tokenVal <$> lexGregLang "" (l >>= recreateToken))

tests :: TestTree
tests = testGroup "Lexer" [testProperty "lexer reversable" lexerReversable]

main = defaultMain tests
