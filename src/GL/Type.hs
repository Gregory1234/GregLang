{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module GL.Type where

import Data.Functor.Identity
import Data.Proxy

class Functor f =>
      TypeFunctor f
  where
  type TypeValue f
  getValTF :: f a -> a
  getTypeTF :: f a -> TypeValue f
  showTypeAnnotation :: Proxy f -> TypeValue f -> String -> String

showAnnotated :: (TypeFunctor f, Show a) => f a -> String
showAnnotated (x :: f a) =
  showTypeAnnotation (Proxy :: Proxy f) (getTypeTF x) (show $ getValTF x)

showAnnotatedString :: (TypeFunctor f) => f String -> String
showAnnotatedString (x :: f String) =
  showTypeAnnotation (Proxy :: Proxy f) (getTypeTF x) (getValTF x)

instance TypeFunctor Identity where
  type TypeValue Identity = ()
  getValTF = runIdentity
  getTypeTF _ = ()
  showTypeAnnotation Proxy () = id
