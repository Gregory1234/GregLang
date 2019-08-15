{-# LANGUAGE OverloadedStrings #-}

module GL.Parser
  ( parseGregLang
  )
where

import           Data.Bool
import           Data.Maybe
import           Data.Tuple.HT
import           Data.Void
import           GL.Data.SyntaxTree
import           GL.Data.Token
import           GL.Data.Ident
import           GL.Type
import qualified Text.Megaparsec               as P
import           Text.Megaparsec                ( (<|>) )
import           Control.Lens            hiding ( (<&>)
                                                , op
                                                )
import           GL.Utils
import           Control.Monad.State

type Parser = P.ParsecT Void [LocToken] (State Integer)

satisfyT :: (Token -> Maybe a) -> Parser a
satisfyT f = fromJust . f . _tokenVal <$> P.satisfy (isJust . f . _tokenVal)

exactT :: Token -> Parser ()
exactT t = P.label (show t) $ void $ P.satisfy ((== t) . _tokenVal)

bracketAny :: Parser () -> Parser () -> Parser a -> Parser a
bracketAny a b c = a *> c <* b

braces :: Parser a -> Parser a
braces = bracketAny (kw "{") (kw "}")

parens :: Parser a -> Parser a
parens = bracketAny (kw "(") (kw ")")

ident :: Parser Ident
ident = P.label "<ident>" $ satisfyT (^? _TIdent)

tident :: Parser ClassName
tident = P.label "<type ident>" $ satisfyT (^? _TTypeIdent)

kw :: Keyword -> Parser ()
kw = exactT . TKeyword

optionL :: Parser [a] -> Parser [a]
optionL = P.option []

maybeCommas :: Parser a -> Parser [a]
maybeCommas a = optionL ((:) <$> a <*> (P.many (kw "," *> a) <|> P.many a))

inc :: (MonadState a m, Num a) => m a
inc = get <* modify (+ 1)

parser :: Parser (AST IType)
parser =
  AST <$> P.many importParser <*> bracketAny (exactT TBegin) P.eof classParser

importParser :: Parser GLImport
importParser = fmap GLImport $ kw "import" *> P.sepBy (show <$> ident) (kw ".")

classParser :: Parser (GLClass IType)
classParser = GLClass <$> (kw "class" *> tident) <*> P.many funParser

funParser :: Parser (GLFun IType)
funParser = undefined

parseGregLang :: FilePath -> [LocToken] -> Either String (AST IType)
parseGregLang p t =
  first P.errorBundlePretty $ flip evalState 0 $ P.runParserT parser p t
