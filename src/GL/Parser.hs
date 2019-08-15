{-# LANGUAGE TupleSections, OverloadedStrings #-}

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

type Parser = P.Parsec Void [LocToken]

type Typed x = x (Maybe GLType)

tokenSatisfy :: (Token -> Maybe a) -> Parser a
tokenSatisfy f =
  fromJust . f . _tokenVal <$> P.satisfy (isJust . f . _tokenVal)

tokenExact :: Token -> Parser ()
tokenExact t = P.label (show t) $ void $ P.satisfy ((== t) . _tokenVal)

bracketAny :: Parser () -> Parser () -> Parser a -> Parser a
bracketAny a b c = a *> c <* b

braces :: Parser a -> Parser a
braces = bracketAny (tokenKeyword "{") (tokenKeyword "}")

parens :: Parser a -> Parser a
parens = bracketAny (tokenKeyword "(") (tokenKeyword ")")

tokenIdent :: Parser Ident
tokenIdent = P.label "<ident>" $ tokenSatisfy (^? _TIdent)

tokenTypeIdent :: Parser ClassName
tokenTypeIdent = P.label "<type ident>" $ tokenSatisfy (^? _TTypeIdent)

tokenKeyword :: Keyword -> Parser ()
tokenKeyword = tokenExact . TKeyword

optionL :: Parser [a] -> Parser [a]
optionL = P.option []

maybeCommas :: Parser a -> Parser [a]
maybeCommas a =
  optionL ((:) <$> a <*> (P.many (tokenKeyword "," *> a) <|> P.many a))

parser :: Parser (Typed AST)
parser = AST [] <$> (tokenExact TBegin *> classParser <* P.eof)

classParser :: Parser (Typed GLClass)
classParser =
  GLClass <$> (tokenKeyword "class" *> tokenTypeIdent) <*> P.many funParser

funParser :: Parser (Typed GLFun)
funParser = uncurry GLFun <$> typeParserClear <*> optionL parseArgs <*> braces
  (P.many statParser)

parseArgs :: Parser [(Maybe GLType, Ident)]
parseArgs = parens (maybeCommas typeParserClear)

statParser :: Parser (Typed GLStat)
statParser =
  P.choice
    $ (SNoOp <$ tokenKeyword ";")
    : (uncurry (bool (<* P.optional (tokenKeyword ";")) id) <$> statParsers)

statParsers :: [(Bool, Parser (Typed GLStat))]
statParsers =
  [ ( True
    , flip SDoWhile
      <$> (tokenKeyword "do" *> statParser)
      <*> (tokenKeyword "while" *> exprParser)
    )
  , (True, SReturn <$> (tokenKeyword "return" *> exprParser))
  , (True, SBreak <$ tokenKeyword "break")
  , (True, SContinue <$ tokenKeyword "continue")
  , ( True
    , uncurry SLet
      <$> (tokenKeyword "let" *> typeParserClear)
      <*> (tokenKeyword "=" *> exprParser)
    )
  , ( True
    , uncurry SSet
      <$> P.try (tokenIdent <&> tokenSatisfy (^? _TKeyword . to show . _Show))
      <*> exprParser
    )
  , (True, SExpr <$> exprParser)
  , ( False
    , SIf <$> (tokenKeyword "if" *> exprParser) <*> statParser <*> P.optional
      (tokenKeyword "else" *> statParser)
    )
  , (False, SWhile <$> (tokenKeyword "while" *> exprParser) <*> statParser)
  , (False, SBraces <$> braces (P.many statParser))
  , ( False
    , uncurry3 SFor
      <$> (   P.try
              (  tokenKeyword "for"
              *> parens (forHelper <* P.optional (tokenKeyword ";"))
              )
          <|> (tokenKeyword "for" *> forHelper)
          )
      <*> statParser
    )
  ]
 where
  forHelper =
    (,,)
      <$> P.option
            SNoOp
            (P.choice (map snd statParsers) <* P.optional (tokenKeyword ";"))
      <*> (exprParser <* P.optional (tokenKeyword ";"))
      <*> P.option SNoOp (P.choice (map snd statParsers))

typeParser :: Parser GLType
typeParser = GLType <$> tokenTypeIdent

typeParserClear :: Parser (Maybe GLType, Ident)
typeParserClear =
  P.try ((Just <$> typeParser) <&> tokenIdent) <|> (Nothing, ) <$> tokenIdent

prefixExprParser :: Parser (Typed GLExpr) -> Parser (Typed GLExpr)
prefixExprParser e =
  (   EPrefix Nothing
    <$> tokenSatisfy (^? _TKeyword . folding (readElem enumerate))
    <*> e
    )
    <|> (   uncurry (set exprType1)
        <$> (P.try ((Just <$> parens typeParser) <&> e) <|> (Nothing, ) <$> e)
        )

exprLevel :: Bool -> [ExprOp] -> Parser (Typed GLExpr) -> Parser (Typed GLExpr)
exprLevel b op e =
  bool foldl (foldr . flip) b (uncurry <$> EOp Nothing) <$> e <*> P.many
    ((,) <$> tokenSatisfy (^? _TKeyword . folding (readElem op)) <*> e)

exprLevels :: [[ExprOp]] -> Parser (Typed GLExpr) -> Parser (Typed GLExpr)
exprLevels = flip (foldr (exprLevel False))

varParser :: Parser (Typed GLExpr) -> Parser (Typed GLExpr)
varParser e = do
  e1 <- e
  ns <- P.many
    (tokenKeyword "." *> tokenIdent <&> optionL
      (parens (maybeCommas exprParser))
    )
  return (foldl (\a (b, l) -> EVar Nothing (Just a) b l) e1 ns)


exprParser :: Parser (Typed GLExpr)
exprParser =
  exprLevels
      [ ["||"]
      , ["^^"]
      , ["&&"]
      , ["|"]
      , ["^"]
      , ["&"]
      , ["==", "!="]
      , ["<", ">", "<=", ">="]
      , ["+", "-"]
      , ["*", "/", "%"]
      ]
    $ prefixExprParser
        (varParser $ P.choice
          [ P.label "int literal"
            $ tokenSatisfy (^? _TIntLit . to (EIntLit Nothing))
          , P.label "float literal"
            $ tokenSatisfy (^? _TFloatLit . to (EFloatLit Nothing))
          , P.label "string literal"
            $ tokenSatisfy (^? _TStringLit . to (EStringLit Nothing))
          , P.label "char literal"
            $ tokenSatisfy (^? _TCharLit . to (ECharLit Nothing))
          , P.label "Ident"
          $   tokenSatisfy (^? _TIdent . to (EVar Nothing Nothing))
          <*> optionL (parens (maybeCommas exprParser))
          , EParen Nothing <$> parens exprParser
          ]
        )

parseGregLang :: FilePath -> [LocToken] -> Either String (Typed AST)
parseGregLang p t = first P.errorBundlePretty (P.runParser parser p t)
