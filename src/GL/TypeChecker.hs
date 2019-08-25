{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections #-}

module GL.TypeChecker
  ( typeCheck
  )
where

import           GL.Type
import           GL.Data.SyntaxTree
import           GL.Data.Ident
import           GL.Utils
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.Except

typeCheck :: AST IType -> Either String (AST GLType)
typeCheck = undefined
