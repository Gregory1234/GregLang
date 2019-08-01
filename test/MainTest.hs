module Main where

import Data.Char
import GL.Data.Token
import GL.Lexer
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
        (((:) <$> elements identBegList <*> listOf (elements identMidList)) `suchThat`
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

newtype TokenStream =
  TokenStream [LocToken]
  deriving (Show)

arbitrarySpelling = return . spellToken

spaceList = filter isSpace [minBound .. maxBound]

identBegList = filter (\c -> isAlpha c || c == '_') [minBound .. maxBound]

identMidList = filter (\c -> isAlphaNum c || c == '_') [minBound .. maxBound]

arbitraryTokenStream :: Int -> P.SourcePos -> Gen [LocToken]
arbitraryTokenStream 0 _ = return []
arbitraryTokenStream n p = do
  tok <- arbitrary
  sd <- arbitrarySpelling tok
  sa <- listOf1 (elements spaceList)
  (LocToken tok p sd sa :) <$>
    arbitraryTokenStream (n - 1) (updatePosString p (sd ++ sa))

instance Arbitrary TokenStream where
  arbitrary = do
    n <- getSize
    sf <- listOf (elements spaceList)
    TokenStream . (LocToken TBegin (P.initialPos "") "" sf :) <$>
      arbitraryTokenStream n (updatePosString (P.initialPos "") sf)
  shrink (TokenStream (b:l)) =
    TokenStream . fixPos (P.initialPos "") . (b :) <$> shrinkTokenStream l
    where
      shrinkTokenStream [] = []
      shrinkTokenStream [x] = pure <$> shrinkLocToken x
      shrinkTokenStream (x:xs) =
        (x : init xs) :
        xs : ((: xs) <$> shrinkLocToken x) ++ ((x :) <$> shrinkTokenStream xs)
      shrinkLocToken (LocToken t p sd sa) =
        (\x ws -> LocToken x p (spellToken x) ws) <$> shrink t <*> shrinkWs sa
      shrinkWs "" = []
      shrinkWs (' ':xs) = [xs]
      shrinkWs (x:xs) = (' ' : xs) : ((x :) <$> shrinkWs xs)
      fixPos _ [] = []
      fixPos p (LocToken t _ sd sa:xs) =
        LocToken t p sd sa : fixPos (updatePosString p (sd ++ sa)) xs

lexerReversable :: TokenStream -> Property
lexerReversable (TokenStream l) =
  Right l === lexGregLang "" (l >>= recreateToken)

tests :: TestTree
tests = testGroup "Lexer" [mkTest "lexer reversable" lexerReversable]
  where
    mkTest s = testProperty s . withMaxSuccess 100

main = defaultMain tests
