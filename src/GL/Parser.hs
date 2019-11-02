{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeFamilies,
  FlexibleContexts, TypeApplications, UndecidableInstances #-}

module GL.Parser
  ( parseGregLang
  )
where

import           Data.Void
import           GL.SyntaxTree
import           GL.Token
import           GL.Ident
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

class Parsable a where
  parser :: Parser a

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

instance Parsable e => Parsable (AST e) where
  parser =
    bracketAny (exactT TBegin) P.eof
      $   AST
      <$> preKw "package" parser
      <*> P.many (preKw "package" parser)
      <*> P.many parser
      <*> P.many parser

instance Parsable Package where
  parser = Package <$> P.sepBy (parser @Ident) (kw ".")

instance Parsable e => Parsable (Class e) where
  parser = Class <$> (kw "class" *> parser) <*> P.many parser

instance Parsable (PartType Integer, Ident) where
  parser = do
    a <- optional parser
    i <- parser
    t <- maybe (NoType <$> inc) (return . NameType) a
    return (t, i)

safeBraces :: Parsable a => Parser [a]
safeBraces = preKw "{" helper
  where helper = (kw "}" $> []) <|> ((:) <$> parser <*> helper)

instance (Parsable (Ident,sig),Parsable cont) => Parsable (Fun sig cont) where
  parser = uncurry Fun <$> parser <*> parser

instance Parsable (t,Ident) => Parsable (Ident,FunSigTyp t) where
  parser = do
    (t, n) <- parser
    a      <- maybeCommas parser
    return (n, FunSigTyp t a)

instance (Parsable (t,Ident),Parsable e) => Parsable [StatTyp t e] where
  parser = safeBraces

instance (Parsable (t,Ident),Parsable e) => Parsable (StatTyp t e) where
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
    , sc $ uncurry SLet <$> preKw "let" parser <*> preKw "=" parser
    , sc $ SReturn <$> preKw "return" parser
    , sc $ kw "break" $> SBreak
    , sc $ kw "continue" $> SContinue
    , kw ";" $> SNoOp
    , SBraces <$> safeBraces
    , sc $ SExpr <$> parser
    ]
    where sc = (<* optional (kw ";"))

instance Parsable Expr where
  parser = do
    e <- P.choice
      [ litParser "<int literal>"    EIntLit    _TIntLit
      , litParser "<float literal>"  EFloatLit  _TFloatLit
      , litParser "<char literal>"   ECharLit   _TCharLit
      , litParser "<string literal>" EStringLit _TStringLit
      , EVar Nothing <$> parser <*> optionL (parens (maybeCommas parser))
      , EParen <$> parens parser
      ]
    ds <- P.many (preKw "." parser <&> optionL (parens (maybeCommas parser)))
    foldM (\e (f, a) -> return $ EVar (Just e) f a) e ds
    where litParser n f g = P.label n $ f <$> satisfyT (^? g)

parseGregLang :: FilePath -> [LocToken] -> Either String UntypedAST
parseGregLang p t =
  first P.errorBundlePretty $ flip evalState 0 $ P.runParserT parser p t
