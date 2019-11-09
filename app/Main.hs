{-# LANGUAGE TypeApplications #-}
module Main where

import           SyntaxTree
import           GregLang
import           Data.Proxy

main = defaultMain (Proxy @UntypedAST)
