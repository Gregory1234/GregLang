module TokenTest where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Text.Megaparsec               as P

import           GL.Token
import           GL.Utils
import           Data.Char
import qualified Data.Text                     as T
import           Data.Text.Arbitrary            ( )

arbitrary' :: Arbitrary a => (a -> Bool) -> Gen a
arbitrary' = suchThat arbitrary

instance Arbitrary Operator where
  arbitrary = elements enumerate

instance Arbitrary OtherSymbol where
  arbitrary = elements enumerate

instance Arbitrary Comparasion where
  arbitrary = elements enumerate

instance Arbitrary BracketType where
  arbitrary = elements enumerate

instance Arbitrary BracketState where
  arbitrary = elements enumerate

instance Arbitrary Symbol where
  arbitrary = oneof
    [ OpSym <$> arbitrary
    , OtherSym <$> arbitrary
    , SetOpSym <$> arbitrary
    , CompOpSym <$> arbitrary
    , BrSym <$> arbitrary
    ]

instance Arbitrary Keyword where
  arbitrary = elements enumerate

instance Arbitrary Ident where
  arbitrary = do
    x <- arbitrary' ((isAlpha &&& isLower) ||| (== '_'))
    let cond = (`notElem` map fromKeyword enumerate) . (toUpper x `T.cons`)
    xs <-
      fmap T.pack (listOf (arbitrary' $ isAlphaNum ||| (== '_')))
        `suchThat` cond
    return . Ident $ x `T.cons` xs

instance Arbitrary ClassName where
  arbitrary =
    ClassName
      .   T.pack
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

mkLocTokens :: String -> [(Token, Text, Text)] -> [LocToken]
mkLocTokens = helper . P.initialPos
 where
  helper _ [] = []
  helper pos ((t, d, a) : xs) =
    LocToken t pos d a : helper (updatePosString pos (d <> a)) xs



tokenTests :: [TestTree]
tokenTests = []
