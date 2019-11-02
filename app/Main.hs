{-# LANGUAGE RecordWildCards #-}

module Main where

import           GL.Args
import           GL.Lexer
import           GL.Parser
import           GL.Utils
import           Control.Monad.Except

main :: IO ()
main = do
  Args {..}   <- getArgs
  fileContent <- readFile inputFileArg
  (either putStrLn pure =<<) . runExceptT $ do
    tok <- liftEither $ lexGregLang inputFileArg fileContent
    lift $ pprint tok
    ast <- liftEither $ parseGregLang inputFileArg tok
    lift $ pprint ast
