{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}

module GL.Lexer.Lexable
  ( module GL.Lexer.Monad
  , module GL.Lexer.Lexable
  )
where

import           Control.Applicative
import           Control.Monad.State
import           GL.Utils
import           GL.Lexer.Monad
import           Data.Char
import qualified Data.Text                     as T


class LexerState s => Lexable s a where
  consume :: Lexer s t a

instance LexerState s => Lexable s Text where
  consume = char '"' *> manyT (charInside '"') <* char '"'

instance LexerState s => Lexable s Ident where
  consume = fmap Ident $ T.cons <$> satisfy (isLower ||| (== '_')) <*> manyT
    (satisfy (isAlphaNum ||| (== '_')))

instance LexerState s => Lexable s ClassName where
  consume = fmap ClassName $ T.cons <$> satisfy isUpper <*> manyT
    (satisfy (isAlphaNum ||| (== '_')))

instance LexerState s => Lexable s Char where
  consume = char '\'' *> charInside '\'' <* char '\''

instance LexerState s => Lexable s Integer where
  consume = read <$> some (satisfy isDigit)

instance LexerState s => Lexable s Double where
  consume = do
    a <- some (satisfy (isDigit ||| (== '.')))
    guard ('.' `elem` a)
    pure (read a)

lexToken :: Lexable s a => Lexer s a ()
lexToken = consume >>= emitToken
