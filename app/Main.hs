{-# LANGUAGE RecordWildCards #-}

module Main where

import           GL.Args
import           GL.Lexer
import           GL.Parser
import           GL.TypeChecker
import           GL.Codegen.LLVM
import           GL.Utils

main :: IO ()
main = do
  Args {..}   <- getArgs
  fileContent <- readFile inputFileArg
  case lexGregLang inputFileArg fileContent of
    (Left  err) -> putStrLn err
    (Right tok) -> pprint tok *> case parseGregLang inputFileArg tok of
      (Left  err) -> putStrLn err
      (Right ast) -> pprint ast *> case typeCheck ast of
        (Left err) -> putStrLn err
        (Right ast') ->
          pprint ast'
            *> let out = codegen inputFileArg ast'
               in
                 putStrLn out
                   *> writeFile
                        (fromMaybe (inputFileArg ++ ".ll") outputFileArg)
                        out
  return ()
