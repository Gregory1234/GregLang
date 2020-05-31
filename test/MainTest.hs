module Main where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit

import           GL.Token.Keyword
import           GL.Utils
import           Data.String

main :: IO ()
main = defaultMain tests

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

tests :: TestTree
tests = testGroup "Tests" [testGroup "Tokens" tokenTests]

tokenTests :: [TestTree]
tokenTests =
  [ testProperty "fromString . getKeyword = id"
                 (\k -> fromString (getKeyword k) === k)
  ]
