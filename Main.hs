{-# LANGUAGE RecordWildCards #-}

module Main where

import GL.Args
import GL.Lexer
import GL.Parser

main :: IO ()
main = do
  Args {..} <- getArgs
  fileContent <- readFile inputFileArg
  case lexGregLang inputFileArg fileContent of
    (Left err) -> putStrLn err
    (Right tok) ->
      print tok *>
      case parseGregLang inputFileArg tok of
        (Left err) -> putStrLn err
        (Right ast) -> print ast
  return ()
