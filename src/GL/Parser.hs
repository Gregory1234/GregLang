{-# LANGUAGE LambdaCase, TupleSections #-}

module GL.Parser
  ( parseGregLang
  )
where

import           Control.Monad
import           Data.Bool
import           Data.Maybe
import           Data.Maybe.HT
import           Data.Tuple.HT
import           Data.Void
import           GL.Data.SyntaxTree
import           GL.Data.Token
import           GL.Type
import qualified Text.Megaparsec               as P
import           Text.Megaparsec                ( (<|>) )

type Parser = P.Parsec Void [LocToken]

type Typed x = x (Maybe GLType)

(<&>) :: Applicative f => f a -> f b -> f (a, b)
a <&> b = (,) <$> a <*> b

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

tokenKeyword :: String -> Parser ()
tokenKeyword = tokenExact . TKeyword . read

parser :: Parser (Typed AST)
parser = AST [] <$> (tokenExact TBegin *> classParser <* P.eof)

classParser :: Parser (Typed GLClass)
classParser =
  GLClass <$> (tokenKeyword "class" *> tokenTypeIdent) <*> P.many funParser

funParser :: Parser (Typed GLFun)
funParser =
  uncurry GLFun <$> typeParserClear <*> P.option [] parseArgs <*> braces
    (P.many statParser)

parseArgs :: Parser [(Maybe GLType, Ident)]
parseArgs =
  parens (P.many typeParserClear <|> P.sepBy typeParserClear (tokenKeyword ","))

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
      (TKeyword a) -> toMaybe (show a `elem` map show setOps) (read $ show a)
      _            -> Nothing
    )

typeParser :: Parser GLType
typeParser = GLType <$> tokenTypeIdent

typeParserClear :: Parser (Maybe GLType, Ident)
typeParserClear =
  P.try ((Just <$> typeParser) <&> tokenIdent) <|> (Nothing, ) <$> tokenIdent

typeParserExpr :: Parser (Typed GLExpr) -> Parser (Typed GLExpr)
typeParserExpr e =
  uncurry changeExprType
    <$> (P.try ((Just <$> parens typeParser) <&> e) <|> (Nothing, ) <$> e)

exprLevel :: Bool -> [ExprOp] -> Parser (Typed GLExpr) -> Parser (Typed GLExpr)
exprLevel b op e =
  bool foldl (foldr . flip) b (uncurry <$> EOp Nothing) <$> e <*> P.many
    (   (,)
    <$> tokenSatisfy
          (\case
            (TKeyword a) ->
              toMaybe (show a `elem` map show op) (read $ show a)
            _ -> Nothing
          )
    <*> e
    )

exprLevels :: [[String]] -> Parser (Typed GLExpr) -> Parser (Typed GLExpr)
exprLevels = flip (foldr (exprLevel False . fmap read))

varParser :: Parser (Typed GLExpr) -> Parser (Typed GLExpr)
varParser e = do
  e1 <- e
  ns <- P.many
    (tokenKeyword "." *> tokenIdent <&> P.option
      []
      (parens (P.many exprParser <|> P.sepBy exprParser (tokenKeyword ",")))
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
    $ typeParserExpr
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
          <*> P.option
                []
                (parens
                  (P.many exprParser <|> P.sepBy exprParser (tokenKeyword ","))
                )
          , EParen Nothing <$> parens exprParser
          ]
        )

parseGregLang :: FilePath -> [LocToken] -> Either String (Typed AST)
parseGregLang p t = case P.runParser parser p t of
  (Left  err) -> Left $ P.errorBundlePretty err
  (Right ast) -> Right ast
