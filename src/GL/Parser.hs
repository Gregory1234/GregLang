{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies,
  FlexibleContexts, TypeApplications #-}

module GL.Parser
  ( parseGregLang
  )
where

import           Data.Void
import           GL.SyntaxTree
import           GL.Token
import           GL.Ident
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

class (P.MonadParsec Void [LocToken] (ParserOf a)) => Parsable a where
  type ParserOf a :: * -> *
  type ParserOf a = Parser
  parser :: ParserOf a a

satisfyT :: (Token -> Maybe a) -> Parser a
satisfyT f = fromJust . f . _tokenVal <$> P.satisfy (isJust . f . _tokenVal)

exactT :: Token -> Parser ()
exactT t = P.label (showPP t) $ void $ P.satisfy ((== t) . _tokenVal)

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

incType :: GLExprU (GLExpr IType) -> Parser (GLExpr IType)
incType a = GLExpr <$> (NumIType <$> inc) <*> pure a

instance Parsable (AST IType) where
  parser =
    bracketAny (exactT TBegin) P.eof
      $   AST
      <$> preKw "package" parser
      <*> P.many parser
      <*> P.many parser
      <*> P.many parser

instance Parsable GLImport where
  parser = GLImport <$> preKw "import" parser

instance Parsable Package where
  parser = Package <$> P.sepBy (showPP <$> parser @Ident) (kw ".")

instance Parsable (GLClass IType) where
  parser =
    GLClass <$> (kw "class" *> parser) <*> P.many parser <*> P.many parser

safeArg :: Parser (IType, Ident)
safeArg = do
  a <- optional parser
  i <- parser
  t <- maybe (NumIType <$> inc) (return . PartIType) a
  return (t, i)

instance Parsable (GLField IType) where
  parser =
    P.try
      $   uncurry GLField
      <$> safeArg
      <*> (optional (preKw "=" parser) <* optional (kw ";"))

safeBraces :: Parser [GLStat IType]
safeBraces = preKw "{" helper
  where helper = (kw "}" $> []) <|> ((:) <$> parser <*> helper)

instance Parsable (GLFun IType) where
  parser =
    uncurry GLFun
      <$> safeArg
      <*> optionL (parens $ maybeCommas safeArg)
      <*> safeBraces

instance Parsable (GLStat IType) where
  parser = P.choice
    [ SIf <$> preKw "if" parser <*> parser <*> optional (preKw "else" parser)
    , (uncurry . uncurry) SFor
    <$> preKw
          "for"
          (   P.try (preKw "(" parser <&> sc parser)
          <&> (parser <* kw ")")
          <|> parser
          <&> sc parser
          <&> parser
          )
    <*> parser
    , SWhile <$> preKw "while" parser <*> parser
    , sc $ flip SDoWhile <$> preKw "do" parser <*> preKw "while" parser
    , sc $ uncurry SLet <$> preKw "let" safeArg <*> preKw "=" parser
    , sc $ P.try (SSet <$> parser <*> so) <*> parser
    , sc $ SReturn <$> preKw "return" parser
    , sc $ kw "break" $> SBreak
    , sc $ kw "continue" $> SContinue
    , kw ";" $> SNoOp
    , SBraces <$> safeBraces
    , sc $ SExpr <$> parser
    ]
   where
    sc = (<* optional (kw ";"))
    so = P.label "<setting operator>"
                 (satisfyT (^? _TKeyword . to showPP . _Pretty))


instance Parsable (GLExpr IType) where
  parser = P.try exprExtParser <|> exprBaseParser

exprBaseParser :: Parser (GLExpr IType)
exprBaseParser =
  GLExpr
    <$> ((PartIType <$> P.try (parens parser)) <|> (NumIType <$> inc))
    <*> exprUBaseParser

exprLevel :: [ExprOp] -> Parser (GLExpr IType) -> Parser (GLExpr IType)
exprLevel op e = do
  e1 <- e
  es <- P.many (bo <&> e)
  foldM (\a (b, c) -> incType $ EOp a b c) e1 es
 where
  bo =
    P.label "<binary operator>" (satisfyT (^? _TKeyword . folding (lexElem op)))

exprLevels :: [[ExprOp]] -> Parser (GLExpr IType) -> Parser (GLExpr IType)
exprLevels = flip $ foldr exprLevel

exprPrefix :: Parser (GLExpr IType) -> Parser (GLExpr IType)
exprPrefix e = (incType =<< (EPrefix <$> po <*> e)) <|> e
 where
  po =
    P.label "<prefix operator>" (satisfyT (^? _TKeyword . to showPP . _Pretty))

exprIfParser :: Parser (GLExpr IType) -> Parser (GLExpr IType)
exprIfParser e = do
  e1 <- e
  es <- P.many (preKw "?" (exprIfParser e) <&> preKw ":" (exprIfParser e))
  foldM (\a (b, c) -> incType $ EIf a b c) e1 es

exprExtParser :: Parser (GLExpr IType)
exprExtParser =
  exprIfParser
    $ exprLevels
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
    $ exprPrefix exprBaseParser

exprUBaseParser :: Parser (GLExprU (GLExpr IType))
exprUBaseParser = do
  e <- P.choice
    [ litParser "<int literal>"    EIntLit    _TIntLit
    , litParser "<float literal>"  EFloatLit  _TFloatLit
    , litParser "<char literal>"   ECharLit   _TCharLit
    , litParser "<string literal>" EStringLit _TStringLit
    , EVar Nothing <$> parser <*> optionL (parens (maybeCommas parser))
    , EParen <$> parens parser
    ]
  ds <- P.many (preKw "." parser <&> optionL (parens (maybeCommas parser)))
  foldM helper e ds
 where
  litParser n f g = P.label n $ f <$> satisfyT (^? g)
  helper a (b, c) = EVar <$> (Just <$> incType a) <*> pure b <*> pure c

parseGregLang :: FilePath -> [LocToken] -> Either String (AST IType)
parseGregLang p t =
  first P.errorBundlePretty $ flip evalState 0 $ P.runParserT parser p t
