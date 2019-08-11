{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

module GL.Utils where

import qualified Data.List.HT                  as L
import           Data.Tree

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
