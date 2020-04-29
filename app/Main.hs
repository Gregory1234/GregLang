{-# LANGUAGE RecordWildCards #-}
module Main where

import           GL.Args
import           GL.Lexer
import           GL.Parser
import           Control.Monad.Except

main = do
  Args {..}   <- getArgs
  fileContent <- readFile inputFileArg
  (either putStrLn pure =<<) . runExceptT $ do
    tok <- liftEither $ lexGregLang inputFileArg fileContent
    lift . putStrLn . unlines . map locTokenPretty $ tok
    ast <- liftEither $ parseGregLang inputFileArg tok
    lift . putStrLn . treePP $ ast
