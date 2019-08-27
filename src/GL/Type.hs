{-# LANGUAGE FlexibleInstances, DerivingVia, OverloadedStrings,
  FlexibleContexts, OverloadedLists, TypeFamilies, StandaloneDeriving #-}

module GL.Type
  ( module GL.Type
  )
where

import           GL.Utils
import           GL.Data.Ident
import           Data.String
import           Data.List.Split
import           Data.List
import           Control.Monad.Except
import           GHC.Exts                       ( IsList(..) )

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

deriving via String instance IsType ClassName

instance IsType Integer where
  showType n x = x ++ " : <" ++ show n ++ ">"

instance (IsType a) => IsType (Maybe a) where
  showType (Just a) = showType a
  showType Nothing  = id

newtype GLPackage = GLPackage { _packagePath :: [String] } deriving Eq

instance Pretty GLPackage where
  showPP (GLPackage s) = intercalate "." s

instance IsList GLPackage where
  type Item GLPackage = String
  fromList = GLPackage
  toList   = _packagePath

data GLType = GLType GLPackage ClassName
  deriving stock Eq

instance Pretty GLType where
  showPP (GLType [] c) = showPP c
  showPP (GLType p  c) = showPP p ++ '.' : showPP c

instance IsType GLType where
  showType (GLType [] c) = showType (showPP c)
  showType (GLType p  c) = showType (showPP p ++ '.' : showPP c)

instance IsString GLType where
  fromString x = helper (splitOn "." x)
   where
    helper []       = GLType [] ""
    helper [a     ] = GLType [] (fromString a)
    helper (a : as) = case helper as of
      (GLType (GLPackage p) c) -> GLType (GLPackage (a : p)) c

data IType =
    NumIType Integer
  | PartIType ClassName
  | ConIType GLType deriving (Eq)

instance IsString IType where
  fromString = ConIType . fromString

instance IsType IType where
  showType (NumIType  n) = showType n
  showType (PartIType t) = showType t
  showType (ConIType  t) = showType t

instance Pretty IType where
  showPP (NumIType  n) = showPP n
  showPP (PartIType t) = showPP t
  showPP (ConIType  t) = showPP t

matchIType :: MonadError String m => IType -> IType -> m IType
matchIType (ConIType a) (ConIType b) | a == b = return $ ConIType a
matchIType (ConIType (GLType p a)) (PartIType b) | a == b =
  return $ ConIType (GLType p a)
matchIType (PartIType a) (ConIType (GLType p b)) | a == b =
  return $ ConIType (GLType p a)
matchIType (PartIType a) (PartIType b) | a == b = return $ PartIType a
matchIType t            (NumIType _)            = return t
matchIType (NumIType _) t                       = return t
matchIType a b =
  throwError $ "Could match types: " ++ showPP a ++ " and " ++ showPP b
