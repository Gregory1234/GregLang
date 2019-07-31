{-# LANGUAGE FlexibleInstances #-}

module GL.Utils where

import qualified Data.List.HT as L
import Data.Tree

class Treeable a where
  toTree :: a -> Tree String

listToTree :: Treeable a => String -> [a] -> Tree String
listToTree s = Node s . map toTree

instance Treeable String where
  toTree s = Node s []

treeShow :: Treeable a => a -> String
treeShow = drawTree . toTree

replaceTabs :: Int -> String -> String
replaceTabs tw = L.replace "\t" (replicate tw ' ')
