{-# LANGUAGE Strict #-}

module GL.Lexer
  ( module GL.Lexer.Monad
  , module GL.Lexer
  )
where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Char
import qualified Data.Text                     as T

import           GL.Lexer.Monad
import           GL.Utils

class Lexable a where
  consume :: Lexer t a

instance Lexable Text where
  consume = char '"' *> manyT (charInside '"') <* char '"'

instance Lexable Ident where
  consume = fmap Ident $ T.cons <$> satisfy (isLower ||| (== '_')) <*> manyT
    (satisfy (isAlphaNum ||| (== '_')))

instance Lexable ClassName where
  consume = fmap ClassName $ T.cons <$> satisfy isUpper <*> manyT
    (satisfy (isAlphaNum ||| (== '_')))

instance Lexable Char where
  consume = char '\'' *> charInside '\'' <* char '\''

instance Lexable Integer where
  consume = read <$> some (satisfy isDigit)

instance Lexable Double where
  consume = do
    a <- some (satisfy (isDigit ||| (== '.')))
    guard ('.' `elem` a)
    pure (read a)

lexToken :: Lexable a => Lexer a ()
lexToken = consume >>= emitToken

runLexer'
  :: LexerT t (Except String) a -> FilePath -> Text -> Either String [LocT t]
runLexer' x fp t =
  runExcept $ snd . fst <$> getLexerT x (LexerState t (emptyLoc fp) 0)

lexGregLang :: Lexable t => FilePath -> Text -> Either String [LocT t]
lexGregLang = runLexer' lexer

lexerChange :: Lexer t a -> LexerT t (Except String) a
lexerChange (LexerT l) =
  LexerT $ \s -> liftEither (maybeToEither "Lexer error" (l s))

lexer :: Lexable t => LexerT t (Except String) ()
lexer = lexerChange $ many lexToken *> eof
