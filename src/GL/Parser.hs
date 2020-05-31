{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies,
  FlexibleContexts, UndecidableInstances, RankNTypes #-}

module GL.Parser
  ( module GL.Parser
  )
where

import           Data.Void
import           GL.Token
import           GL.SyntaxTree
import qualified Text.Megaparsec               as P
import           Control.Lens            hiding ( (<&>)
                                                , op
                                                )
import           GL.Utils
import           Control.Monad.State

type Parser = P.ParsecT Void [LocToken] (State Integer)

class Parsable a where
  parser :: Parser a

satisfyT :: (Token -> Maybe a) -> Parser a
satisfyT f = fromJust . f . tokenVal <$> P.satisfy (isJust . f . tokenVal)

exactT :: Token -> Parser ()
exactT t = P.label (tokenPretty t) $ void $ P.satisfy ((== t) . tokenVal)

bracketAny :: Parser () -> Parser () -> Parser a -> Parser a
bracketAny a b c = a *> c <* b

parens :: Parser a -> Parser a
parens = bracketAny (kw "(") (kw ")")

instance Parsable Ident where
  parser = P.label "<ident>" $ satisfyT (^? _TIdent)

instance Parsable ClassName where
  parser = P.label "<type ident>" $ satisfyT (^? _TTypeIdent)

kw :: Keyword -> Parser ()
kw = exactT . TKeyword

preKw :: Keyword -> Parser a -> Parser a
preKw k a = kw k *> a

optionL :: Parser [a] -> Parser [a]
optionL = P.option []

maybeCommas :: Parser a -> Parser [a]
maybeCommas a =
  optionL ((:) <$> a <*> optionL (P.some (preKw "," a) <|> P.some a))


inc :: (MonadState a m, Num a) => m a
inc = get <* modify (+ 1)

litParser :: String -> Prism' Token a -> Parser a
litParser n g = P.label n (satisfyT (^? g))

instance Parsable Integer where
  parser = litParser "<int literal>" _TIntLit

instance Parsable Double where
  parser = litParser "<double literal>" _TFloatLit

instance Parsable String where
  parser = litParser "<string literal>" _TStringLit

instance Parsable Char where
  parser = litParser "<char literal>" _TCharLit

safeBraces :: Parsable a => Parser [a]
safeBraces = preKw "{" helper
  where helper = (kw "}" $> []) <|> ((:) <$> parser <*> helper)

instance Parsable AST where
  parser = undefined

parseGregLang :: FilePath -> [LocToken] -> Either String AST
parseGregLang p t =
  first P.errorBundlePretty $ flip evalState 0 $ P.runParserT parser p t
