{-# LANGUAGE FlexibleInstances #-}

module GL.Treeable where

import Data.Tree

class Treeable a where
  toTree :: a -> Tree String

listToTree :: Treeable a => String -> [a] -> Tree String
listToTree s = Node s . map toTree

instance Treeable String where
  toTree s = Node s []

treeShow :: Treeable a => a -> String
treeShow = drawTree . toTree
