{-# LANGUAGE RecordWildCards #-}
module Main where

import GL.Args
import GL.Lexer

main :: IO ()
main = do
  (Args {..}) <- getArgs
  fileContent <- readFile inputFileArg
  case (lexGregLang inputFileArg fileContent) of
    (Left err) -> print err
    (Right tok) -> print tok
  return ()
