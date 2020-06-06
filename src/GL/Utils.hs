{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}

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
  )
where

import qualified Data.List.HT                  as L
import           Data.Tree
import           Data.List
import           Data.Bifunctor
import           Data.Function
import           Data.Functor                   ( ($>) )
import           Data.Maybe.HT
import           Data.Maybe
import           Control.Monad
import           Control.Lens
import           Data.Functor.Identity
import           Control.Monad.Except
import           Data.Foldable
import           Control.Applicative
import           Debug.Trace
import           Data.String


-- | Like 'trace' but returns both the shown value and a third value.
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
  toTree :: a -> Tree String

-- | Convert a list of values to 'Tree' of 'String's with a given root.
listToTree
  :: Treeable a
  => String -- ^ Root of the tree
  -> [a]
  -> Tree String
listToTree s = Node s . map toTree

-- | Convert a list of values to 'Forest' of 'String's.
--
-- > toForest = map toTree
toForest :: Treeable a => [a] -> Forest String
toForest = map toTree

instance Treeable String where
  toTree s = Node s []

instance Treeable (Tree String) where
  toTree = id

-- | Pretty printing of a tree.
treePP :: Treeable a => a -> String
treePP = drawTree . toTree

-- | Replaces tabs in a 'String' with a given amount of spaces using 'L.replace'.
replaceTabs
  :: Int -- ^ Anount of spaces
  -> String
  -> String
replaceTabs tw = L.replace "\t" (replicate tw ' ')

infixl 2 |||

-- | 'liftA2' on '||'
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = liftA2 (||)

infixl 3 &&&

-- | 'liftA2' on '&&'
(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = liftA2 (&&)

-- | Split a list on another list.
--
-- >>> breakList "wor" "helloworld"
-- ("hello", "world")
--
-- >>> breakList "abc" "helloworld"
-- ("helloworld", "")
breakList :: Eq a => [a] -> [a] -> ([a], [a])
breakList xs ys | xs `isPrefixOf` ys = ([], ys)
breakList _  []                      = ([], [])
breakList xs (y : ys)                = first (y :) $ breakList xs ys

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

-- | Safe version of 'head'.
headError :: MonadError a m => a -> [b] -> m b
headError x []      = throwError x
headError _ (x : _) = return x

-- | Safe version of 'only'.
onlyEither :: a -> [b] -> Either a b
onlyEither _ [x] = Right x
onlyEither x _   = Left x

joinFun :: (Monad m) => m (a -> m b) -> a -> m b
joinFun f a = ($ a) =<< f

foldMapA :: (Applicative f, Monoid m, Foldable t) => (a -> f m) -> t a -> f m
foldMapA f = foldr (liftA2 mappend . f) (pure mempty)

-- | Change a 'Maybe' value to any 'Alternative'.
maybeToAlt :: Alternative m => Maybe a -> m a
maybeToAlt (Just a) = pure a
maybeToAlt Nothing  = empty

-- | Append lists fairly.
fairAppend :: [a] -> [a] -> [a]
fairAppend (x : xs) (y : ys) = x : y : fairAppend xs ys
fairAppend []       y        = y
fairAppend x        []       = x

-- | Take the only element of a list.
only :: [a] -> a
only [x] = x
only []  = error "GL.Utils.only: []"
only _   = error "GL.Utils.only: too big"

infixl 3 |>

-- | Add a default value for an 'Alternative'.
(|>) :: Alternative f => f a -> a -> f a
f |> a = f <|> pure a

-- | An identifier beggining with lowercase.
newtype Ident =
  Ident { getIdent :: String }
  deriving newtype (Eq, Ord, IsString, Treeable, Show)

-- | An identifier beggining with uppercase.
newtype ClassName =
  ClassName { getClassName :: String }
  deriving newtype (Eq, Ord, IsString, Treeable, Show)
