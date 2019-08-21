{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GL.Utils
  ( Tree(..)
  , first
  , second
  , on
  , ($>)
  , void
  , module Data.Default.Class
  , module GL.Utils
  )
where

import qualified Data.List.HT                  as L
import           Data.Tree
import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read
import           Data.List
import           Data.Bifunctor
import           Data.Function
import           Data.Functor                   ( ($>) )
import           Data.Maybe.HT
import           Control.Monad
import           Data.Default.Class

class Treeable a where
  toTree :: a -> Tree String

listToTree :: Treeable a => String -> [a] -> Tree String
listToTree s = Node s . map toTree

instance Treeable String where
  toTree s = Node s []

instance Treeable (Tree String) where
  toTree = id

treeShow :: Treeable a => a -> String
treeShow = drawTree . toTree

newtype PrettyTree t =
  PrettyTree t
  deriving (Treeable)

instance Treeable t => Show (PrettyTree t) where
  show = treeShow

replaceTabs :: Int -> String -> String
replaceTabs tw = L.replace "\t" (replicate tw ' ')

appDb :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
appDb f g h x = f (g x) (h x)

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = appDb (||)

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = appDb (&&)

newtype ClearShow = ClearShow String

instance Show ClearShow where
  show (ClearShow x) = x

readPrecGather :: Read a => ReadS (String, a)
readPrecGather = RP.readP_to_S (RP.gather (readPrec_to_P readPrec 0))

listToEither :: b -> [a] -> Either b a
listToEither b []      = Left b
listToEither _ (x : _) = Right x

breakList :: Eq a => [a] -> [a] -> ([a], [a])
breakList xs ys | xs `isPrefixOf` ys = ([], ys)
breakList _  []                      = ([], [])
breakList xs (y : ys)                = first (y :) $ breakList xs ys

infixl 4 <&>
(<&>) :: Applicative f => f a -> f b -> f (a, b)
a <&> b = (,) <$> a <*> b

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

readElem :: (Show a, Show b, Read b) => [b] -> a -> Maybe b
readElem l a = toMaybe (show a `elem` map show l) (read $ show a)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither x Nothing  = Left x

headEither :: a -> [b] -> Either a b
headEither x []      = Left x
headEither _ (x : _) = Right x

eitherConcat :: Monoid m => [Either a m] -> Either a m
eitherConcat (Left  x : _ ) = Left x
eitherConcat (Right x : xs) = case eitherConcat xs of
  Left  a -> Left a
  Right a -> Right (a <> x)
eitherConcat [] = Right mempty

setAt :: (Integral n, Default a) => n -> a -> [a] -> [a]
setAt n _ _ | n < 0 = error "GL.Utils.setAt: negative index"
setAt 0 a []        = [a]
setAt n a []        = def : setAt (n - 1) a []
setAt 0 a (_ : xs)  = a : xs
setAt n a (x : xs)  = x : setAt (n - 1) a xs

getAt :: (Integral n, Default a) => n -> [a] -> a
getAt n _ | n < 0 = error "GL.Utils.getAt: negative index"
getAt _ []        = def
getAt 0 (x : _ )  = x
getAt n (_ : xs)  = getAt (n - 1) xs

tryTillStableM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
tryTillStableM f a = do
  b <- f a
  if b == a then return a else tryTillStableM f b
