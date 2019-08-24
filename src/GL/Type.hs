{-# LANGUAGE FlexibleInstances, DerivingVia #-}

module GL.Type
  ( module GL.Type
  )
where

import           GL.Utils
import           GL.Data.Ident
import           Data.String

class IsType t where
  showType :: t -> String -> String

showTypeTree :: IsType t => t -> Tree String -> Tree String
showTypeTree t (Node a b) = Node (showType t a) b

showTypeP :: (IsType t, Pretty a) => t -> a -> String
showTypeP t = showType t . showPP

instance IsType () where
  showType () = id

instance IsType String where
  showType s x = x ++ " : " ++ s

instance IsType Integer where
  showType n x = x ++ " : <" ++ show n ++ ">"

instance (IsType a) => IsType (Maybe a) where
  showType (Just a) = showType a
  showType Nothing  = id

newtype GLType = GLType ClassName
  deriving stock (Eq)
  deriving (IsType,IsString) via String
  deriving Pretty via ClearString

data IType =
    NumberIType Integer
  | ConcreteIType GLType deriving (Eq)

instance IsType IType where
  showType (NumberIType   n) = showType n
  showType (ConcreteIType t) = showType t
