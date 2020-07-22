{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module GL.Utils
  ( module Data.Tree
  , module Data.Bifunctor
  , on
  , ($>)
  , void
  , join
  , zipWithM
  , module Data.Functor.Identity
  , module Data.Maybe
  , module Data.Foldable
  , toMaybe
  , liftA2
  , (<|>)
  , module Debug.Trace
  , module GL.Utils
  , Text
  )
where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Functor                   ( ($>) )
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Maybe.HT
import           Data.String
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Tree
import           Debug.Trace


-- | Like 'trace' but prints both the message and the value.
--
-- >>> traceN "msg " 12
-- msg 12
-- 12
traceN
  :: Show a
  => String -- ^ the prepended message
  -> a
  -> a
traceN s a = trace (s ++ show a) a

-- | Conversion of values to 'Tree' of 'String's.
class Treeable a where
  {-# MINIMAL toTree #-}
  -- | Convert a value to 'Tree' of 'String's.
  toTree :: a -> Tree Text

-- | Convert a list of values to 'Tree' of 'String's with a given root.
listToTree
  :: Treeable a
  => Text -- ^ Root of the tree
  -> [a]
  -> Tree Text
listToTree s = Node s . map toTree

instance Treeable Text where
  toTree s = Node s []

instance Treeable (Tree Text) where
  toTree = id

-- | Pretty printing of a tree.
treePP :: Treeable a => a -> Text
treePP = T.pack . drawTree . fmap T.unpack . toTree

-- | A tree with no branches.
empTree :: Text -> Tree Text
empTree = toTree

-- | Replaces tabs in a 'String' with a given amount of spaces.
replaceTabs
  :: Int -- ^ Anount of spaces
  -> Text
  -> Text
replaceTabs tw = T.replace "\t" (T.replicate tw " ")

infixl 2 |||

-- | 'liftA2' on '||'
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = liftA2 (||)

infixl 3 &&&

-- | 'liftA2' on '&&'
(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = liftA2 (&&)

infixl 4 <&>
-- | 'liftA2' on the pair constructor.
(<&>) :: Applicative f => f a -> f b -> f (a, b)
(<&>) = liftA2 (,)

-- | List all elements of a 'Bounded' 'Enum'.
enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

-- | Change a 'Maybe' into an 'Either'.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither x Nothing  = Left x

-- | An identifier beggining with lowercase.
newtype Ident =
  Ident { getIdent :: Text }
  deriving newtype (Eq, Ord, IsString, Treeable, Show)

-- | An identifier beggining with uppercase.
newtype ClassName =
  ClassName { getClassName :: Text }
  deriving newtype (Eq, Ord, IsString, Treeable, Show)

-- | 'show' for 'Text'.
showT :: Show a => a -> Text
showT = T.pack . show

-- | Zero or more 'Char's.
manyT :: Alternative f => f Char -> f Text
manyT v = many_v
 where
  many_v = some_v <|> pure T.empty
  some_v = liftA2 T.cons v many_v

liftDefaultOne :: a -> (a -> a -> b) -> Maybe a -> Maybe a -> Maybe b
liftDefaultOne _ f (Just a) (Just b) = Just (f a b)
liftDefaultOne a f Nothing  (Just b) = Just (f a b)
liftDefaultOne b f (Just a) Nothing  = Just (f a b)
liftDefaultOne _ _ Nothing  Nothing  = Nothing
