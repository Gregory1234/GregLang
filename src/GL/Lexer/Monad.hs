{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module GL.Lexer.Monad
  ( module GL.Loc
  , module GL.Lexer.Monad
  )
where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Char
import qualified Data.Text                     as T

import           GL.Loc
import           GL.Utils

data LexerState = LexerState
    { _lexRest :: Text
    , _lexLoc :: Loc
    , _lexTokId :: Int
    }
  deriving (Eq, Show)

makeLenses ''LexerState

newtype LexerT t m a
  = LexerT { getLexerT :: LexerState -> m ((a, [LocT t]), LexerState) }
    deriving
      ( Functor
      , Applicative
      , Monad
      , Alternative
      , MonadPlus
      , MonadFail
      , MonadWriter [LocT t]
      , MonadState LexerState
      ) via (WriterT [LocT t] (StateT LexerState m))

type Lexer t = LexerT t Maybe

runLexer :: Lexer t a -> FilePath -> Text -> Maybe [LocT t]
runLexer x fp t = snd . fst <$> getLexerT x (LexerState t (emptyLoc fp) 0)

evalLexer :: Lexer t a -> FilePath -> Text -> Maybe a
evalLexer x fp t = fst . fst <$> getLexerT x (LexerState t (emptyLoc fp) 0)

emitToken :: t -> Lexer t ()
emitToken t = do
  consumeSpace
  use lexLoc >>= (\l -> tell [LocT t l])
  lexLoc %= commit
  lexTokId += 1

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

consumeSpace :: Lexer t ()
consumeSpace = do
  dat <- use lexRest
  let (ws, r) = spanSpace Whitespace dat
  lexLoc %= addAfter ws
  lexRest .= r

eof :: Lexer t ()
eof = do
  dat <- use lexRest
  guard (T.null dat)

char :: Char -> Lexer t Char
char a = satisfy (== a)

charInside :: Char -> Lexer t Char
charInside del = (char '\\' *> satisfyMaybe escChar)
  <|> satisfy (`notElem` ['\\', del])
 where
  escChar '\\'         = Just '\\'
  escChar 't'          = Just '\t'
  escChar 'n'          = Just '\n'
  escChar 'r'          = Just '\r'
  escChar x | x == del = Just del
  escChar _            = Nothing

string :: Text -> Lexer t Text
string str = scanToken (fmap (\(a, b) -> (a, a, b)) . scan str)
 where
  scan s dat = case T.uncons s of
    Nothing      -> Just (s, dat)
    Just (x, xs) -> do
      (y, ys) <- T.uncons dat
      guard (x == y)
      (a, b) <- scan xs ys
      return (T.cons x a, b)

scanToken :: (Text -> Maybe (a, Text, Text)) -> Lexer t a
scanToken scan = do
  dat              <- use lexRest
  (Just (x, y, z)) <- pure $ scan dat
  lexLoc %= addDuring y
  lexRest .= z
  pure x

satisfy :: (Char -> Bool) -> Lexer t Char
satisfy f = satisfyMaybe $ \c -> toMaybe (f c) c

satisfyMaybe :: (Char -> Maybe a) -> Lexer t a
satisfyMaybe f = scanToken $ \t -> do
  (x, xs) <- T.uncons t
  (, T.singleton x, xs) <$> f x

enumToken :: (Enum a, Bounded a) => (a -> Text) -> Lexer t a
enumToken f = asum $ fmap (\x -> string (f x) $> x) enumerate
