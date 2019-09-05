{-# LANGUAGE FlexibleInstances, DerivingVia, OverloadedStrings,
  FlexibleContexts, OverloadedLists, TypeFamilies, StandaloneDeriving,
  TemplateHaskell, GeneralizedNewtypeDeriving #-}

module GL.Type
  ( module GL.Type
  )
where

import           GL.Utils
import           GL.Ident
import           Data.String
import           Data.List.Split
import           Data.List
import           Control.Lens
import           GHC.Exts                       ( IsList(..) )

class IsType t where
  showType :: t -> String -> String
  fromType :: GLType -> t

showTypeTree :: IsType t => t -> Tree String -> Tree String
showTypeTree t (Node a b) = Node (showType t a) b

showTypeP :: (IsType t, Pretty a) => t -> a -> String
showTypeP t = showType t . showPP

instance IsType () where
  showType () = id
  fromType _ = ()

instance IsType String where
  showType s x = x ++ " : " ++ s
  fromType = showPP

deriving via String instance IsType ClassName

newtype Package = Package { packagePath :: [String] }
  deriving newtype (Eq, Show)

instance Pretty Package where
  showPP (Package s) = intercalate "." s

instance IsList Package where
  type Item Package = String
  fromList = Package
  toList   = packagePath

data GLType = GLType { typePackage :: Package, typeClass :: ClassName}
  deriving stock (Eq, Show)

instance Pretty GLType where
  showPP (GLType [] c) = showPP c
  showPP (GLType p  c) = showPP p ++ '.' : showPP c

instance IsType GLType where
  showType (GLType [] c) = showType (showPP c)
  showType (GLType p  c) = showType (showPP p ++ '.' : showPP c)
  fromType = id

instance IsString GLType where
  fromString x = helper (splitOn "." x)
   where
    helper []       = GLType [] ""
    helper [a     ] = GLType [] (fromString a)
    helper (a : as) = case helper as of
      (GLType (Package p) c) -> GLType (Package (a : p)) c

data IType =
    NumIType Integer
  | PartIType ClassName
  | ConIType GLType deriving (Eq, Show)

instance IsString IType where
  fromString = ConIType . fromString

instance IsType IType where
  showType (NumIType  n) = showType ("<" ++ show n ++ ">")
  showType (PartIType t) = showType t
  showType (ConIType  t) = showType t
  fromType = ConIType

instance Pretty IType where
  showPP (NumIType  n) = showPP n
  showPP (PartIType t) = showPP t
  showPP (ConIType  t) = showPP t

makePrisms ''IType
