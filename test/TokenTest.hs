module TokenTest where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Text.Megaparsec               as P

import           GL.Token
import           GL.Utils
import           Data.String
import           Data.Char

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

instance Arbitrary BracketState where
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
  arbitrary = do
    x <- arbitrary' ((isAlpha &&& isLower) ||| (== '_'))
    let cond = (`notElem` map fromReservedKeyword enumerate) . (toUpper x :)
    xs <- listOf (arbitrary' $ isAlphaNum ||| (== '_')) `suchThat` cond
    return $ Ident (x : xs)

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

mkLocTokens :: String -> [(Token, String, String)] -> [LocToken]
mkLocTokens = helper . P.initialPos
 where
  helper _ [] = []
  helper pos ((t, d, a) : xs) =
    LocToken t pos d a : helper (updatePosString pos (d ++ a)) xs



tokenTests :: [TestTree]
tokenTests =
  [ testProperty "fromString . fromKeyword == id"
                 (\k -> fromString (fromKeyword k) === k)
  ]
