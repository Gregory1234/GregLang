{-# LANGUAGE FlexibleInstances, DerivingVia, OverloadedStrings #-}

module GL.Type
  ( module GL.Type
  )
where

import           GL.Utils
import           GL.Data.Ident
import           Data.String
import           Data.List.Split
import           Data.List

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

newtype GLPackage = GLPackage { _packagePath :: [String] } deriving Eq

instance Pretty GLPackage where
  showPP (GLPackage s) = intercalate "." s

data GLType = GLType (Maybe GLPackage) ClassName
  deriving stock Eq

instance IsType GLType where
  showType (GLType (Just p) c) = showType (showPP p ++ '.' : showPP c)
  showType (GLType Nothing  c) = showType (showPP c)

instance IsString GLType where
  fromString x = helper (splitOn "." x)
   where
    helper []       = GLType Nothing ""
    helper [a     ] = GLType Nothing (fromString a)
    helper (a : as) = case helper as of
      (GLType (Just (GLPackage p)) c) -> GLType (Just (GLPackage (a : p))) c
      (GLType Nothing              c) -> GLType (Just (GLPackage [a])) c

data IType =
    NumberIType Integer
  | ConcreteIType GLType deriving (Eq)

instance IsType IType where
  showType (NumberIType   n) = showType n
  showType (ConcreteIType t) = showType t
