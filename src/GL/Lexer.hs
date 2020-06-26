{-# LANGUAGE Strict #-}

module GL.Lexer
  ( module GL.Lexer
  , module GL.Lexer.Lexable
  )
where

import           GL.Lexer.Lexable
import           Control.Monad.Except
import           Control.Applicative
import           GL.Utils

runLexer'
  :: LexerT t (Except String) a -> FilePath -> String -> Either String [t]
runLexer' x fn str =
  runExcept . fmap (snd . fst) . getLexerT x $ emptyLexerState fn str

lexGregLang :: Lexable t => FilePath -> String -> Either String [t]
lexGregLang = runLexer' lexer

lexerChange :: Lexer t a -> LexerT t (Except String) a
lexerChange (LexerT l) =
  LexerT $ \s -> liftEither (maybeToEither "Lexer error" (l s))

lexer :: Lexable t => LexerT t (Except String) ()
lexer = lexerChange $ many lexToken *> eof
