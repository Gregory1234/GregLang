module Main where

import GL.Args

main :: IO ()
main = do
  a <- getArgs
  print a
  return ()
