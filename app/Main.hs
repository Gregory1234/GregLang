{-# LANGUAGE TypeApplications #-}
module Main where

import           GregLang.SyntaxTree
import           GregLang
import           Data.Proxy

main = defaultMain (Proxy @UntypedAST)
