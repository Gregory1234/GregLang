{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import           Test.Tasty

import           LexerTest
import           TokenTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
  "Tests"
  [testGroup "Tokens" tokenTests, testGroup "Lexer" lexerTests]
