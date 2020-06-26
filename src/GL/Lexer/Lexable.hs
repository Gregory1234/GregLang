{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}

module GL.Lexer.Lexable
  ( module GL.Lexer.Lexable
  )
where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Text.Megaparsec               as P
import           Control.Lens
import           GL.Utils
import           Data.Char

updatePosString :: P.SourcePos -> String -> P.SourcePos
updatePosString p []          = p
updatePosString p ('\t' : xs) = updatePosString
  (p { P.sourceColumn = P.sourceColumn p <> P.defaultTabWidth })
  xs
updatePosString p ('\n' : xs) = updatePosString
  (p { P.sourceLine = P.sourceLine p <> P.pos1, P.sourceColumn = P.pos1 })
  xs
updatePosString p (_ : xs) =
  updatePosString (p { P.sourceColumn = P.sourceColumn p <> P.pos1 }) xs

data LexerState = LexerState
  { _lexerPos :: P.SourcePos
  , _lexerSpellingDuring :: String
  , _lexerSpellingAfter :: String
  , _lexerData :: String
  , _hadBegin :: Bool
  }
  deriving (Show)

makeLenses ''LexerState

newtype LexerT t m a = LexerT {getLexerT :: LexerState -> m ((a,[t]),LexerState)}
  deriving
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadPlus
    , MonadFail
    , MonadWriter [t]
    , MonadState LexerState
    ) via (WriterT [t] (StateT LexerState m))

type Lexer t = LexerT t Maybe

emptyLexerState :: FilePath -> String -> LexerState
emptyLexerState fn str = LexerState { _lexerPos            = P.initialPos fn
                                    , _lexerSpellingDuring = ""
                                    , _lexerSpellingAfter  = ""
                                    , _lexerData           = str
                                    , _hadBegin            = False
                                    }

runLexerS :: Lexer t a -> LexerState -> Maybe [t]
runLexerS x = fmap (snd . fst) . getLexerT x

runLexer :: Lexer t a -> FilePath -> String -> Maybe [t]
runLexer x fn str = runLexerS x $ emptyLexerState fn str

evalLexerS :: Lexer t a -> LexerState -> Maybe a
evalLexerS x = fmap (fst . fst) . getLexerT x

evalLexer :: Lexer t a -> FilePath -> String -> Maybe a
evalLexer x fn str = evalLexerS x $ emptyLexerState fn str

emitToken :: t -> Lexer t ()
emitToken t = tell [t] >> modify
  (\s@LexerState {..} -> s
    { _lexerSpellingDuring = ""
    , _lexerSpellingAfter  = ""
    , _lexerPos            = updatePosString
                               _lexerPos
                               (_lexerSpellingDuring ++ _lexerSpellingAfter)
    }
  )

data WhitespaceType = Whitespace | LineComment | BlockComment

spanSpace :: WhitespaceType -> String -> (String, String)
spanSpace Whitespace ('/' : '/' : xs) =
  let (w, r) = spanSpace LineComment xs in ('/' : '/' : w, r)
spanSpace Whitespace ('/' : '*' : xs) =
  let (w, r) = spanSpace BlockComment xs in ('/' : '*' : w, r)
spanSpace Whitespace (x : xs) | isSpace x =
  let (w, r) = spanSpace Whitespace xs in (x : w, r)
spanSpace Whitespace xs = ("", xs)
spanSpace LineComment ('\n' : xs) =
  let (w, r) = spanSpace Whitespace xs in ('\n' : w, r)
spanSpace BlockComment ('*' : '/' : xs) =
  let (w, r) = spanSpace Whitespace xs in ('*' : '/' : w, r)
spanSpace t (x : xs) = let (w, r) = spanSpace t xs in (x : w, r)
spanSpace _ []       = ("", "")

consumeSpace :: Lexer t ()
consumeSpace = do
  dat <- use lexerData
  let (ws, rest) = spanSpace Whitespace dat
  lexerSpellingAfter %= (<> ws)
  lexerData .= rest

eof :: Lexer t ()
eof = do
  dat <- use lexerData
  guard (null dat)
  pure ()

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

string :: String -> Lexer t String
string str = scanToken (fmap (\(a, b) -> (a, a, b)) . scan str)
 where
  scan []       dat = pure ([], dat)
  scan (x : xs) dat = do
    (y : ys) <- pure dat
    guard (x == y)
    (a, b) <- scan xs ys
    pure (x : a, b)

scanToken :: (String -> Maybe (a, String, String)) -> Lexer t a
scanToken scan = do
  dat              <- use lexerData
  (Just (x, y, z)) <- pure $ scan dat
  lexerSpellingDuring %= (<> y)
  lexerData .= z
  pure x

satisfy :: (Char -> Bool) -> Lexer t Char
satisfy f = satisfyMaybe $ \c -> toMaybe (f c) c

satisfyMaybe :: (Char -> Maybe a) -> Lexer t a
satisfyMaybe f = scanToken $ \case
  []       -> Nothing
  (x : xs) -> (, [x], xs) <$> f x


enumToken :: (Enum a, Bounded a) => (a -> String) -> Lexer t a
enumToken f = asum $ fmap (\x -> string (f x) $> x) enumerate


class Lexable a where
  consume :: Lexer t a

instance Lexable String where
  consume = char '"' *> many (charInside '"') <* char '"'

instance Lexable Ident where
  consume = fmap Ident $ (:) <$> satisfy (isLower ||| (== '_')) <*> many
    (satisfy (isAlphaNum ||| (== '_')))

instance Lexable ClassName where
  consume = fmap ClassName $ (:) <$> satisfy isUpper <*> many
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
