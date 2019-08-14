{-# LANGUAGE FlexibleInstances, DerivingVia #-}

module GL.Type
  ( IsType(..)
  , showTypeTree
  , showTypeShow
  , GLType(..)
  )
where

import           Data.Tree
import           GL.Utils
import           GL.Data.Ident

class IsType t where
  showType :: t -> String -> String

showTypeTree :: IsType t => t -> Tree String -> Tree String
showTypeTree t (Node a b) = Node (showType t a) b

showTypeShow :: (IsType t, Show a) => t -> a -> String
showTypeShow t = showType t . show

instance IsType () where
  showType () = id

instance IsType String where
  showType s x = x ++ " : " ++ s

instance (IsType a) => IsType (Maybe a) where
  showType (Just a) = showType a
  showType Nothing  = id

newtype GLType = GLType ClassName
  deriving stock Eq
  deriving IsType via String
  deriving Show via ClearShow
