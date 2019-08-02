{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module GL.Type where

import Data.Functor.Identity

class Functor f =>
      TypeFunctor f
  where
  data TypeValue f
  getValTF :: f a -> a
  getTypeTF :: f a -> TypeValue f
  showTypeAnnotation :: TypeValue f -> String -> String

instance TypeFunctor Identity where
  data TypeValue Identity = NoType
  getValTF = runIdentity
  getTypeTF _ = NoType
  showTypeAnnotation NoType = id
