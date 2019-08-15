{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}

module GL.Parser
  ( parseGregLang
  )
where

import           Control.Monad
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
import           Control.Lens                   ( set )
import           GL.Utils
import           Text.Read                      ( readMaybe )

type Parser = P.Parsec Void [LocToken]

type Typed x = x (Maybe GLType)

tokenSatisfy :: (Token -> Maybe a) -> Parser a
tokenSatisfy f = fromJust . f . tokenVal <$> P.satisfy (isJust . f . tokenVal)

tokenExact :: Token -> Parser ()
tokenExact t = P.label (show t) $ void $ P.satisfy ((== t) . tokenVal)

bracketAny :: Parser () -> Parser () -> Parser a -> Parser a
bracketAny a b c = a *> c <* b

braces :: Parser a -> Parser a
braces = bracketAny (tokenKeyword "{") (tokenKeyword "}")

parens :: Parser a -> Parser a
parens = bracketAny (tokenKeyword "(") (tokenKeyword ")")

tokenIdent :: Parser Ident
tokenIdent = P.label "<ident>" $ tokenSatisfy
  (\case
    (TIdent s) -> Just s
    _          -> Nothing
  )

tokenTypeIdent :: Parser ClassName
tokenTypeIdent = P.label "<type ident>" $ tokenSatisfy
  (\case
    (TTypeIdent s) -> Just s
    _              -> Nothing
  )

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
  , (True, uncurry SSet <$> P.try (tokenIdent <&> setHelper) <*> exprParser)
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
  setHelper = tokenSatisfy
    (\case
      (TKeyword a) -> readMaybe $ show a
      _            -> Nothing
    )

typeParser :: Parser GLType
typeParser = GLType <$> tokenTypeIdent

typeParserClear :: Parser (Maybe GLType, Ident)
typeParserClear =
  P.try ((Just <$> typeParser) <&> tokenIdent) <|> (Nothing, ) <$> tokenIdent

prefixExprParser :: Parser (Typed GLExpr) -> Parser (Typed GLExpr)
prefixExprParser e =
  (   EPrefix Nothing
    <$> tokenSatisfy
          (\case
            (TKeyword a) -> readElem enumerate a
            _            -> Nothing
          )
    <*> e
    )
    <|> (   uncurry (set exprType1)
        <$> (P.try ((Just <$> parens typeParser) <&> e) <|> (Nothing, ) <$> e)
        )

exprLevel :: Bool -> [ExprOp] -> Parser (Typed GLExpr) -> Parser (Typed GLExpr)
exprLevel b op e =
  bool foldl (foldr . flip) b (uncurry <$> EOp Nothing) <$> e <*> P.many
    (   (,)
    <$> tokenSatisfy
          (\case
            (TKeyword a) -> readElem op a
            _            -> Nothing
          )
    <*> e
    )

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
          [ P.label "int literal" $ tokenSatisfy
            (\case
              (TIntLit a) -> Just (EIntLit Nothing a)
              _           -> Nothing
            )
          , P.label "float literal" $ tokenSatisfy
            (\case
              (TFloatLit a) -> Just (EFloatLit Nothing a)
              _             -> Nothing
            )
          , P.label "string literal" $ tokenSatisfy
            (\case
              (TStringLit a) -> Just (EStringLit Nothing a)
              _              -> Nothing
            )
          , P.label "char literal" $ tokenSatisfy
            (\case
              (TCharLit a) -> Just (ECharLit Nothing a)
              _            -> Nothing
            )
          , P.label "Ident"
          $   tokenSatisfy
                (\case
                  (TIdent a) -> Just (EVar Nothing Nothing a)
                  _          -> Nothing
                )
          <*> optionL (parens (maybeCommas exprParser))
          , EParen Nothing <$> parens exprParser
          ]
        )

parseGregLang :: FilePath -> [LocToken] -> Either String (Typed AST)
parseGregLang p t = case P.runParser parser p t of
  (Left  err) -> Left $ P.errorBundlePretty err
  (Right ast) -> Right ast
