{-# LANGUAGE RecordWildCards #-}
module Main where

import           GL.Args
import           GL.Lexer
import           GL.Token
import           GL.Parser
import           GL.Utils
import           Control.Monad.Except
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

main = do
  Args {..}   <- getArgs
  fileContent <- T.readFile inputFileArg
  (either putStrLn pure =<<) . runExceptT $ do
    tok <- liftEither $ lexGregLang inputFileArg fileContent
    lift . T.putStrLn . T.unlines . map locTokenPretty $ tok
    ast <- liftEither $ parseGregLang inputFileArg tok
    lift . T.putStrLn . treePP $ ast
