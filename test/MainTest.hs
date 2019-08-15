{-# LANGUAGE Strict #-}

module Main where

import           Data.Char
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           GL.Data.Token
import           GL.Lexer
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.QuickCheck
import qualified Text.Megaparsec               as P
import           GL.Utils
import           GL.Data.Ident

instance Arbitrary Keyword where
  arbitrary = elements enumerate
  shrink (Keyword "if") = []
  shrink _              = [Keyword "if"]

keywords :: [String]
keywords = map show (enumerate :: [Keyword])

instance Arbitrary Ident where
  arbitrary =
    Ident
      <$> (((:) <$> elements identBegList <*> listOf (elements identMidList))
          `suchThat` (`notElem` keywords)
          )
  shrink (Ident x) = Ident <$> filter (`notElem` keywords) (shrinkIdent x)
   where
    shrinkIdent []  = []
    shrinkIdent [x] = pure <$> shrinkChar x
    shrinkIdent (x : xs) =
      (x : init xs) : xs : (pure <$> shrinkChar x) ++ ((x :) <$> shrinkIdent xs)
    shrinkChar x | x == 'a'  = []
                 | x == 'A'  = ['a']
                 | x == '1'  = ['a']
                 | isUpper x = ['A']
                 | isLower x = ['a']
                 | isDigit x = ['1']
                 | otherwise = ['a']

instance Arbitrary ClassName where
  arbitrary =
    ClassName
      <$> (((:) <$> elements typeIdentBegList <*> listOf (elements identMidList)
           )
          `suchThat` (`notElem` map show keywords)
          )
  shrink (ClassName x) = ClassName <$> shrinkIdent x
   where
    shrinkIdent []  = []
    shrinkIdent [x] = pure <$> shrinkChar x
    shrinkIdent (x : xs) =
      (x : init xs) : xs : (pure <$> shrinkChar x) ++ ((x :) <$> shrinkIdent xs)
    shrinkChar x | x == 'a'  = []
                 | x == 'A'  = []
                 | x == '1'  = ['a']
                 | isUpper x = ['A']
                 | isLower x = ['a']
                 | isDigit x = ['1']
                 | otherwise = ['a']

instance Arbitrary Token where
  arbitrary = oneof
    [ TIdent <$> arbitrary
    , TTypeIdent <$> arbitrary
    , TIntLit <$> (arbitrary `suchThat` (>= 0))
    , TFloatLit <$> (arbitrary `suchThat` (>= 0))
    , TKeyword <$> arbitrary
    , TStringLit <$> arbitrary
    , TCharLit <$> arbitrary
    ]
  shrink TBegin                  = []
  shrink (TIdent     x         ) = TIdent <$> shrink x
  shrink (TTypeIdent x         ) = TTypeIdent <$> shrink x
  shrink (TIntLit    x         ) = TIntLit <$> filter (>= 0) (shrink x)
  shrink (TFloatLit  x         ) = TFloatLit <$> filter (>= 0) (shrink x)
  shrink (TKeyword   x         ) = TKeyword <$> shrink x
  shrink (TCharLit   x         ) = TCharLit <$> shrink x
  shrink (TStringLit ""        ) = []
  shrink (TStringLit "a"       ) = [TStringLit []]
  shrink (TStringLit ('a' : xs)) = [TStringLit xs, TStringLit ('a' : init xs)]
  shrink (TStringLit [_       ]) = [TStringLit [], TStringLit "a"]
  shrink (TStringLit (x : xs)) =
    TStringLit xs
      : TStringLit (x : init xs)
      : TStringLit ('a' : xs)
      : (TStringLit . (x :) . (\(TStringLit x) -> x) <$> shrink (TStringLit xs))

newtype TokenStream =
  TokenStream (NonEmpty LocToken)
  deriving (Show)

arbitrarySpelling = return . spellToken

spaceList = filter isSpace enumerate

identBegList = filter (isAlpha &&& isLower ||| (== '_')) enumerate

typeIdentBegList = filter (isAlpha &&& isUpper) enumerate

identMidList = filter (isAlphaNum ||| (== '_')) enumerate

arbitraryTokenStream :: Int -> P.SourcePos -> Gen [LocToken]
arbitraryTokenStream 0 _ = return []
arbitraryTokenStream n p = do
  tok <- arbitrary
  sd  <- arbitrarySpelling tok
  sa  <- listOf1 (elements spaceList)
  (LocToken tok p sd sa :)
    <$> arbitraryTokenStream (n - 1) (updatePosString p (sd ++ sa))

instance Arbitrary TokenStream where
  arbitrary = do
    n  <- getSize
    sf <- listOf (elements spaceList)
    TokenStream
      .   (LocToken TBegin (P.initialPos "") "" sf :|)
      <$> arbitraryTokenStream n (updatePosString (P.initialPos "") sf)
  shrink (TokenStream (b :| l)) =
    TokenStream . fixPos (P.initialPos "") . (b :|) <$> shrinkTokenStream l
   where
    shrinkTokenStream []  = []
    shrinkTokenStream [x] = pure <$> shrinkLocToken x
    shrinkTokenStream (x : xs) =
      (x : init xs)
        :  xs
        :  ((: xs) <$> shrinkLocToken x)
        ++ ((x :) <$> shrinkTokenStream xs)
    shrinkLocToken (LocToken t p _ sa) =
      let f x = LocToken x p (spellToken x)
      in  (flip f sa <$> shrink t) ++ (f t <$> shrinkWs sa)
    shrinkWs ""         = []
    shrinkWs " "        = [""]
    shrinkWs (' ' : xs) = [xs, ' ' : init xs]
    shrinkWs [_       ] = [" "]
    shrinkWs (x : xs  ) = (' ' : xs) : ((x :) <$> shrinkWs xs)
    fixPos' _ [] = []
    fixPos' p (LocToken t _ sd sa : xs) =
      LocToken t p sd sa : fixPos' (updatePosString p (sd ++ sa)) xs
    fixPos p (LocToken t _ sd sa :| xs) =
      LocToken t p sd sa :| fixPos' (updatePosString p (sd ++ sa)) xs

lexerReversable :: TokenStream -> Property
lexerReversable (TokenStream l) =
  Right (NE.toList l) === lexGregLang "" (NE.toList l >>= recreateToken)

tests :: TestTree
tests = testGroup "Lexer" [mkTest "lexer reversable" lexerReversable]
  where mkTest s = testProperty s . withMaxSuccess 200

main = defaultMain tests
