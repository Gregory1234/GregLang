{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module GregLang
  ( module GregLang
  )
where

import           GL.Args
import           GL.Lexer
import           GL.Utils
import           GL.SyntaxTree
import           Control.Monad.Except
import           GL.Parser
import           Data.Proxy

defaultMain :: IsSyntax a => Proxy a -> IO ()
defaultMain (Proxy :: Proxy a) = do
  Args {..}   <- getArgs
  fileContent <- readFile inputFileArg
  (either putStrLn pure =<<) . runExceptT $ do
    tok <- liftEither $ lexGregLang inputFileArg fileContent
    lift $ pprint tok
    (ast :: a) <- liftEither $ parseGregLang inputFileArg tok
    lift $ pprint ast
