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
