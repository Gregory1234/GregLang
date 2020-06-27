{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}

module GL.Lexer.Monad
  ( module GL.Lexer.Monad
  )
where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Lens
import           GL.Utils
import           Data.Char
import qualified Data.Text                     as T

class LexerState s where
  rest :: Lens' s Text
  addDuring :: Text -> s -> s
  addAfter :: Text -> s -> s
  commit :: s -> s


newtype LexerT s t m a
  = LexerT { getLexerT :: s -> m ((a, [t]), s) }
    deriving
      ( Functor
      , Applicative
      , Monad
      , Alternative
      , MonadPlus
      , MonadFail
      , MonadWriter [t]
      , MonadState s
      ) via (WriterT [t] (StateT s m))

type Lexer s t = LexerT s t Maybe

runLexerS :: Lexer s t a -> s -> Maybe [t]
runLexerS x = fmap (snd . fst) . getLexerT x

evalLexerS :: Lexer s t a -> s -> Maybe a
evalLexerS x = fmap (fst . fst) . getLexerT x

emitToken :: LexerState s => t -> Lexer s t ()
emitToken t = tell [t] >> modify commit

data WhitespaceType = Whitespace | LineComment | BlockComment

spanSpace :: WhitespaceType -> Text -> (Text, Text)
spanSpace Whitespace (T.splitAt 2 -> ("//", xs)) =
  let (w, r) = spanSpace LineComment xs in ("//" <> w, r)
spanSpace Whitespace (T.splitAt 2 -> ("/*", xs)) =
  let (w, r) = spanSpace BlockComment xs in ("/*" <> w, r)
spanSpace Whitespace (T.uncons -> Just (x, xs)) | isSpace x =
  let (w, r) = spanSpace Whitespace xs in (x `T.cons` w, r)
spanSpace Whitespace xs = ("", xs)
spanSpace LineComment (T.uncons -> Just ('\n', xs)) =
  let (w, r) = spanSpace Whitespace xs in ('\n' `T.cons` w, r)
spanSpace BlockComment (T.splitAt 2 -> ("*/", xs)) =
  let (w, r) = spanSpace Whitespace xs in ("*/" <> w, r)
spanSpace t (T.uncons -> Just (x, xs)) =
  let (w, r) = spanSpace t xs in (x `T.cons` w, r)
spanSpace _ _ = ("", "")

consumeSpace :: LexerState s => Lexer s t ()
consumeSpace = do
  dat <- use rest
  let (ws, r) = spanSpace Whitespace dat
  modify (addAfter ws)
  rest .= r

eof :: LexerState s => Lexer s t ()
eof = do
  dat <- use rest
  guard (T.null dat)
  pure ()

char :: LexerState s => Char -> Lexer s t Char
char a = satisfy (== a)

charInside :: LexerState s => Char -> Lexer s t Char
charInside del = (char '\\' *> satisfyMaybe escChar)
  <|> satisfy (`notElem` ['\\', del])
 where
  escChar '\\'         = Just '\\'
  escChar 't'          = Just '\t'
  escChar 'n'          = Just '\n'
  escChar 'r'          = Just '\r'
  escChar x | x == del = Just del
  escChar _            = Nothing

string :: LexerState s => Text -> Lexer s t Text
string str = scanToken (fmap (\(a, b) -> (a, a, b)) . scan str)
 where
  scan s dat = case T.uncons s of
    Nothing      -> Just (s, dat)
    Just (x, xs) -> do
      (y, ys) <- T.uncons dat
      guard (x == y)
      (a, b) <- scan xs ys
      return (T.cons x a, b)

scanToken :: LexerState s => (Text -> Maybe (a, Text, Text)) -> Lexer s t a
scanToken scan = do
  dat              <- use rest
  (Just (x, y, z)) <- pure $ scan dat
  modify (addDuring y)
  rest .= z
  pure x

satisfy :: LexerState s => (Char -> Bool) -> Lexer s t Char
satisfy f = satisfyMaybe $ \c -> toMaybe (f c) c

satisfyMaybe :: LexerState s => (Char -> Maybe a) -> Lexer s t a
satisfyMaybe f = scanToken $ \t -> do
  (x, xs) <- T.uncons t
  (, T.singleton x, xs) <$> f x

enumToken :: (Enum a, Bounded a, LexerState s) => (a -> Text) -> Lexer s t a
enumToken f = asum $ fmap (\x -> string (f x) $> x) enumerate

newtype EmptyState = EmptyState Text

instance LexerState EmptyState where
  rest      = coerced
  addDuring = const id
  addAfter  = const id
  commit    = id
