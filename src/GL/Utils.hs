{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

module GL.Utils
  ( Tree(..)
  , first
  , second
  , on
  , ($>)
  , void
  , Treeable(..)
  , listToTree
  , treeShow
  , PrettyTree(..)
  , replaceTabs
  , appDb
  , (|||)
  , (&&&)
  , lookupInv
  , ClearShow(..)
  , readPrecGather
  , listToEither
  , breakList
  , (<&>)
  , enumerate
  , readElem
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

lookupInv :: (Eq a) => a -> [(b, a)] -> Maybe b
lookupInv _ [] = Nothing
lookupInv k ((x, y) : xs) | k == y    = Just x
                          | otherwise = lookupInv k xs

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
