{-# LANGUAGE OverloadedStrings #-}

module GL.Parser
  ( parseGregLang
  )
where

import           Data.Maybe
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
import           Control.Monad
import           Control.Monad.State
import           Control.Applicative

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

preKw :: Keyword -> Parser a -> Parser a
preKw k a = kw k *> a

optionL :: Parser [a] -> Parser [a]
optionL = P.option []

maybeCommas :: Parser a -> Parser [a]
maybeCommas a =
  optionL ((:) <$> a <*> optionL (P.some (preKw "," a) <|> P.some a))

inc :: (MonadState a m, Num a) => m a
inc = get <* modify (+ 1)

incType :: GLExprU (GLExpr IType) -> Parser (GLExpr IType)
incType a = GLExpr <$> (NumberIType <$> inc) <*> pure a

parser :: Parser (AST IType)
parser =
  AST <$> P.many importParser <*> bracketAny (exactT TBegin) P.eof classParser

importParser :: Parser GLImport
importParser = GLImport <$> preKw "import" packageParser

packageParser :: Parser GLPackage
packageParser = GLPackage <$> P.sepBy (show <$> ident) (kw ".")

classParser :: Parser (GLClass IType)
classParser = GLClass <$> (kw "class" *> tident) <*> P.many funParser

typeParser :: Parser GLType
typeParser = GLType <$> tident

safeArg :: Parser (IType, Ident)
safeArg = do
  a <- optional typeParser
  i <- ident
  t <- maybe (NumberIType <$> inc) (return . ConcreteIType) a
  return (t, i)

funParser :: Parser (GLFun IType)
funParser =
  uncurry GLFun
    <$> safeArg
    <*> optionL (parens $ maybeCommas safeArg)
    <*> braces (P.many statParser)

statParser :: Parser (GLStat IType)
statParser = P.choice
  [ SIf <$> preKw "if" exprParser <*> statParser <*> optional
    (preKw "else" statParser)
  , (uncurry . uncurry) SFor <$> preKw "for" forHelper <*> statParser
  , SWhile <$> preKw "while" exprParser <*> statParser
  , sc $ flip SDoWhile <$> preKw "do" statParser <*> preKw "while" exprParser
  , sc $ uncurry SLet <$> preKw "let" safeArg <*> preKw "=" exprParser
  , sc $ P.try (SSet <$> ident <*> so) <*> exprParser
  , sc $ SReturn <$> preKw "return" exprParser
  , sc $ kw "break" $> SBreak
  , sc $ kw "continue" $> SContinue
  , kw ";" $> SNoOp
  , SBraces <$> braces (P.many statParser)
  , sc $ SExpr <$> exprParser
  ]
 where
  sc = (<* optional (kw ";"))
  so = P.label "<setting operator>" (satisfyT (^? _TKeyword . to show . _Show))
  forHelper =
    P.try (preKw "(" statParser <&> sc exprParser)
      <&> (statParser <* kw ")")
      <|> statParser
      <&> sc exprParser
      <&> statParser


exprParser :: Parser (GLExpr IType)
exprParser = exprExtParser <|> exprBaseParser

exprBaseParser :: Parser (GLExpr IType)
exprBaseParser =
  GLExpr
    <$> ((ConcreteIType <$> parens typeParser) <|> (NumberIType <$> inc))
    <*> exprUBaseParser

exprLevel :: [ExprOp] -> Parser (GLExpr IType) -> Parser (GLExpr IType)
exprLevel op e = do
  e1 <- e
  es <- P.many (bo <&> e)
  foldM helper e1 es
 where
  helper a (b, c) = incType $ EOp a b c
  bo = P.label "<binary operator>"
               (satisfyT (^? _TKeyword . folding (readElem op)))

exprLevels :: [[ExprOp]] -> Parser (GLExpr IType) -> Parser (GLExpr IType)
exprLevels = flip $ foldr exprLevel

exprPrefix :: Parser (GLExpr IType) -> Parser (GLExpr IType)
exprPrefix e = (incType =<< (EPrefix <$> po <*> e)) <|> e
 where
  po = P.label "<prefix operator>" (satisfyT (^? _TKeyword . to show . _Show))

exprExtParser :: Parser (GLExpr IType)
exprExtParser =
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
    $   exprPrefix
    $   incType
    =<< exprUBaseParser

exprUBaseParser :: Parser (GLExprU (GLExpr IType))
exprUBaseParser = do
  e <- P.choice
    [ litParser "<int literal>"    EIntLit    _TIntLit
    , litParser "<float literal>"  EFloatLit  _TFloatLit
    , litParser "<char literal>"   ECharLit   _TCharLit
    , litParser "<string literal>" EStringLit _TStringLit
    , EVar Nothing <$> ident <*> optionL (parens (maybeCommas exprParser))
    , EParen <$> parens exprParser
    ]
  ds <- P.many (preKw "." ident <&> optionL (parens (maybeCommas exprParser)))
  foldM helper e ds
 where
  litParser n f g = P.label n $ f <$> satisfyT (^? g)
  helper a (b, c) = EVar <$> (Just <$> incType a) <*> pure b <*> pure c

parseGregLang :: FilePath -> [LocToken] -> Either String (AST IType)
parseGregLang p t =
  first P.errorBundlePretty $ flip evalState 0 $ P.runParserT parser p t
