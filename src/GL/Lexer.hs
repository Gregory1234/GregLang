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

runLexer' :: LexerT s t (Except String) a -> s -> Either String [t]
runLexer' x = runExcept . fmap (snd . fst) . getLexerT x

lexGregLang :: Lexable s t => s -> Either String [t]
lexGregLang = runLexer' lexer

lexerChange :: Lexer s t a -> LexerT s t (Except String) a
lexerChange (LexerT l) =
  LexerT $ \s -> liftEither (maybeToEither "Lexer error" (l s))

lexer :: Lexable s t => LexerT s t (Except String) ()
lexer = lexerChange $ many lexToken *> eof
