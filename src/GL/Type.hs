{-# LANGUAGE FlexibleInstances, DerivingVia, OverloadedStrings,
  FlexibleContexts #-}

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

instance Pretty GLType where
  showPP (GLType (Just p) c) = showPP p ++ '.' : showPP c
  showPP (GLType Nothing  c) = showPP c

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

tryType :: MonadError String m => IType -> m GLType
tryType (NumberIType   _) = throwError "Couldn't get type"
tryType (ConcreteIType t) = return t

matchIType :: MonadError String m => IType -> IType -> m IType
matchIType (NumberIType   a) (NumberIType   b) = return $ NumberIType a
matchIType (ConcreteIType a) (NumberIType   b) = return $ ConcreteIType a
matchIType (NumberIType   a) (ConcreteIType b) = return $ ConcreteIType b
matchIType (ConcreteIType a) (ConcreteIType b)
  | a == b
  = return $ ConcreteIType a
  | otherwise
  = throwError $ "Cannot match types: " ++ showPP a ++ " and " ++ showPP b

matchIType' :: MonadError String m => IType -> GLType -> m GLType
matchIType' a b = matchIType a (ConcreteIType b) >>= tryType
