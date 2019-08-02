{-# LANGUAGE LambdaCase #-}

module GL.Parser
  ( parseGregLang
  ) where

import Control.Monad
import Data.Functor.Identity
import Data.Maybe
import Data.Void
import GL.Data.SyntaxTree
import GL.Data.Token
import qualified Text.Megaparsec as P

type Parser = P.Parsec Void [LocToken]

type ParserError = P.ParseErrorBundle [LocToken] Void

tokenSatisfy :: (Token -> Maybe a) -> Parser a
tokenSatisfy f = fromJust . f . tokenVal <$> P.satisfy (isJust . f . tokenVal)

tokenExact :: Token -> Parser ()
tokenExact t = P.label (show t) $ void $ P.satisfy ((== t) . tokenVal)

bracketAny :: Parser () -> Parser () -> Parser a -> Parser a
bracketAny a b c = a *> c <* b

braces :: Parser a -> Parser a
braces = bracketAny (tokenKeyword "{") (tokenKeyword "}")

tokenIdent :: Parser String
tokenIdent =
  P.label "<ident>" $
  tokenSatisfy
    (\case
       (TIdent s) -> Just s
       _ -> Nothing)

tokenKeyword :: String -> Parser ()
tokenKeyword = tokenExact . TKeyword . read

parser :: Parser (AST Identity)
parser = AST [] <$> (tokenExact TBegin *> classParser <* P.eof)

classParser :: Parser (GLClass Identity)
classParser =
  GLClass <$> (tokenKeyword "class" *> tokenIdent) <*> P.many funParser

funParser :: Parser (GLFun Identity)
funParser =
  GLFun <$> (Identity <$> tokenIdent) <*> pure [] <*> braces (P.many statParser)

statParser :: Parser (GLStat Identity)
statParser =
  P.choice
    [ (\a b -> maybe (SIf a b) (SIfElse a b)) <$>
      (tokenKeyword "if" *> exprParser) <*>
      statParser <*>
      P.optional (tokenKeyword "else" *> statParser)
    , SBraces <$> braces (P.many statParser)
    , SExpr <$> exprParser
    ]

exprParser :: Parser (GLExpr Identity)
exprParser =
  GLExpr . Identity <$>
  P.choice
    [ EIntLit <$>
      tokenSatisfy
        (\case
           (TIntLit a) -> Just a
           _ -> Nothing)
    , EFloatLit <$>
      tokenSatisfy
        (\case
           (TFloatLit a) -> Just a
           _ -> Nothing)
    , EParen <$> bracketAny (tokenKeyword "(") (tokenKeyword ")") exprParser
    ]

parseGregLang :: FilePath -> [LocToken] -> Either String (AST Identity)
parseGregLang p t =
  case P.runParser parser p t of
    (Left err) -> Left $ P.errorBundlePretty err
    (Right ast) -> Right ast
