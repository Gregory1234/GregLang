{-# LANGUAGE MultiParamTypeClasses #-}
module GL.Type
  ( module GL.Type
  )
where

import           GL.Parser
import           GL.Utils

class IsType t where
  parserType :: Parsable a => Parser (t,a)
  parserTypeParens :: Parsable a => Parser (t,a)
  parserNoType :: Parser t
  typeAnnotate :: t -> String -> String

typeTree :: (IsType t, Treeable a) => t -> a -> Tree String
typeTree t x = let (Node a b) = toTree x in Node (typeAnnotate t a) b

typeTreePP :: (IsType t, Treeable a) => t -> a -> String
typeTreePP t a = drawTree (typeTree t a)

class Typed e where
  getType :: IsType t => e t -> t

class (Monad m, IsType t) => TypeMonad m t where

class TypeCheckable a where
  typeCheck :: TypeMonad m t => a t -> m ()
