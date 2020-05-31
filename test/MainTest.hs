{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit

import           GL.Token
import           GL.Lexer
import           GL.Utils
import           Data.String
import           Data.Char

main :: IO ()
main = defaultMain tests

arbitrary' :: Arbitrary a => (a -> Bool) -> Gen a
arbitrary' = suchThat arbitrary

instance Arbitrary Operator where
  arbitrary = elements enumerate

instance Arbitrary OtherSymbol where
  arbitrary = elements enumerate

instance Arbitrary Comparasion where
  arbitrary = elements enumerate

instance Arbitrary ReservedKeyword where
  arbitrary = elements enumerate

instance Arbitrary BracketType where
  arbitrary = elements enumerate

instance Arbitrary Keyword where
  arbitrary = oneof
    [ OKeyword <$> arbitrary
    , SKeyword <$> arbitrary
    , OSKeyword <$> arbitrary
    , CKeyword <$> arbitrary
    , RKeyword <$> arbitrary
    , BKeyword <$> arbitrary
    ]

instance Arbitrary Ident where
  arbitrary =
    Ident
      <$> ((:) <$> arbitrary' ((isAlpha &&& isLower) ||| (== '_')) <*> listOf
            (arbitrary' $ isAlphaNum ||| (== '_'))
          )

instance Arbitrary ClassName where
  arbitrary =
    ClassName
      <$> ((:) <$> arbitrary' (isAlpha &&& isUpper) <*> listOf
            (arbitrary' $ isAlphaNum ||| (== '_'))
          )

instance Arbitrary Token where
  arbitrary = oneof
    [ TIdent <$> arbitrary
    , TTypeIdent <$> arbitrary
    , TStringLit <$> arbitrary
    , TIntLit <$> arbitrary' (>= 0)
    , TFloatLit <$> arbitrary' (>= 0)
    , TCharLit <$> arbitrary
    , TKeyword <$> arbitrary
    ]

tests :: TestTree
tests = testGroup
  "Tests"
  [testGroup "Tokens" tokenTests, testGroup "Lexer" lexerTests]

tokenTests :: [TestTree]
tokenTests =
  [ testProperty "fromString . getKeyword == id"
                 (\k -> fromString (getKeyword k) === k)
  ]

lexerTests :: [TestTree]
lexerTests =
  [ testProperty "lexS . getKeyword == id" (\k -> lexS (getKeyword k) === k)
  , testProperty "lexS . spellToken == id" (\k -> lexS (spellToken k) === k)
  ]
