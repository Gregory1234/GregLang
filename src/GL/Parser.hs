{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module GL.Parser
  ( module GL.Parser
  , P.try
  , P.optional
  )
where

import           Control.Lens            hiding ( (<&>)
                                                , op
                                                )
import           Control.Monad.State
import qualified Data.Text                     as T
import           Data.Void
import qualified Text.Megaparsec               as P

import           GL.Token
import           GL.Utils

type Parser = P.ParsecT Void [LocT Token] (State Integer)

class Parsable a where
  parser :: Parser a

satisfyT :: (Token -> Maybe a) -> Parser a
satisfyT f = fromJust . f . _tokVal <$> P.satisfy (isJust . f . _tokVal)

exactT :: Token -> Parser ()
exactT t =
  P.label (T.unpack $ tokenPretty t) $ void $ P.satisfy ((== t) . _tokVal)

bracketAny :: Parser () -> Parser () -> Parser a -> Parser a
bracketAny a b c = a *> c <* b

parens :: Parser a -> Parser a
parens = bracketAny (sm "(") (sm ")")

instance Parsable (Maybe Operator) where
  parser = P.label "<set operator>" $ satisfyT (^? _TSymbol . _SetOpSym)

instance Parsable Ident where
  parser = P.label "<ident>" $ satisfyT (^? _TIdent)

instance Parsable ClassName where
  parser = P.label "<type ident>" $ satisfyT (^? _TTypeIdent)

sm :: Symbol -> Parser ()
sm = exactT . TSymbol

preSm :: Symbol -> Parser a -> Parser a
preSm s a = sm s *> a

kw :: Keyword -> Parser ()
kw = exactT . TKeyword

preKw :: Keyword -> Parser a -> Parser a
preKw k a = kw k *> a

optionL :: Parser [a] -> Parser [a]
optionL = P.option []

maybeCommas :: Parser a -> Parser [a]
maybeCommas a =
  optionL ((:) <$> a <*> optionL (P.some (preSm "," a) <|> P.some a))

finished :: Parser a -> Parser a
finished = bracketAny (exactT TBegin) P.eof

inc :: (MonadState a m, Num a) => m a
inc = get <* modify (+ 1)

litParser :: String -> Prism' Token a -> Parser a
litParser n g = P.label n (satisfyT (^? g))

instance Parsable Integer where
  parser = litParser "<int literal>" _TIntLit

instance Parsable Double where
  parser = litParser "<double literal>" _TFloatLit

instance Parsable Text where
  parser = litParser "<string literal>" _TStringLit

instance Parsable Char where
  parser = litParser "<char literal>" _TCharLit

instance Parsable Bool where
  parser = kw "true" $> True <|> kw "false" $> False

safeBraces :: Parsable a => Parser [a]
safeBraces = preSm "{" helper
  where helper = (sm "}" $> []) <|> ((:) <$> parser <*> helper)

parseGregLang :: Parsable a => FilePath -> [LocT Token] -> Either String a
parseGregLang p t =
  first P.errorBundlePretty $ flip evalState 0 $ P.runParserT parser p t
