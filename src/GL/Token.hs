{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module GL.Token
  ( module GL.Token
  , module GL.Token.Keyword
  )
where

import           Control.Monad
import           Data.Char
import           Data.Function
import qualified Data.List.NonEmpty            as NE
import           Data.Proxy
import           GL.Utils
import qualified Text.Megaparsec               as P
import           Text.Megaparsec                ( SourcePos(..) )
import           Control.Lens
import           GL.Token.Keyword
import           GL.Lexer
import qualified Data.Text                     as T

updatePosString :: SourcePos -> Text -> SourcePos
updatePosString = T.foldl $ \p -> \case
  '\t' -> p { sourceColumn = sourceColumn p <> P.defaultTabWidth }
  '\n' -> p { sourceLine = sourceLine p <> P.pos1, sourceColumn = P.pos1 }
  _    -> p { sourceColumn = sourceColumn p <> P.pos1 }

data LocTokenState = LocTokenState
  { _lexerPos :: SourcePos
  , _lexerSpellingDuring :: Text
  , _lexerSpellingAfter :: Text
  , _lexerData :: Text
  , _hadBegin :: Bool
  }
  deriving (Show)

makeLenses ''LocTokenState

instance LexerState LocTokenState where
  rest      = lexerData
  addDuring = (lexerSpellingDuring <>~)
  addAfter  = (lexerSpellingAfter <>~)
  commit s@LocTokenState {..} = s
    { _lexerPos            = updatePosString
                               _lexerPos
                               (_lexerSpellingDuring <> _lexerSpellingAfter)
    , _lexerSpellingDuring = ""
    , _lexerSpellingAfter  = ""
    }

emptyLexerState :: FilePath -> Text -> LocTokenState
emptyLexerState fp str = LocTokenState { _lexerPos            = P.initialPos fp
                                       , _lexerSpellingDuring = ""
                                       , _lexerSpellingAfter  = ""
                                       , _lexerData           = str
                                       , _hadBegin            = False
                                       }

data Token
  = TBegin
  | TIdent Ident
  | TTypeIdent ClassName
  | TStringLit Text
  | TIntLit Integer
  | TFloatLit Double
  | TCharLit Char
  | TSymbol Symbol
  | TKeyword Keyword
  deriving (Eq, Ord, Show)

spellToken :: Token -> Text
spellToken TBegin         = ""
spellToken (TIdent     x) = getIdent x
spellToken (TTypeIdent x) = getClassName x
spellToken (TStringLit x) = showT x
spellToken (TIntLit    x) = showT x
spellToken (TFloatLit  x) = showT x
spellToken (TCharLit   x) = showT x
spellToken (TSymbol    x) = fromSymbol x
spellToken (TKeyword   x) = fromKeyword x

tokenPretty :: Token -> Text
tokenPretty TBegin         = "<begin>"
tokenPretty (TIdent     s) = "<ident " <> getIdent s <> ">"
tokenPretty (TTypeIdent s) = "<type ident " <> getClassName s <> ">"
tokenPretty (TStringLit s) = "<string " <> showT s <> ">"
tokenPretty (TIntLit    s) = "<int " <> showT s <> ">"
tokenPretty (TFloatLit  s) = "<float " <> showT s <> ">"
tokenPretty (TCharLit   s) = "<char " <> showT s <> ">"
tokenPretty (TSymbol    s) = showT (fromSymbol s)
tokenPretty (TKeyword   s) = showT (fromKeyword s)

instance Lexable LocTokenState Token where
  consume = asum
    [ (uses hadBegin not >>= guard >> hadBegin .= True) $> TBegin
    , do
      a <- consume
      b <- use lexerData
      guard (T.null b || not (isAlphaNum $ T.head b))
      return $ TKeyword a
    , TSymbol <$> consume
    , TStringLit <$> consume
    , TFloatLit <$> consume
    , TIntLit <$> consume
    , TCharLit <$> consume
    , TIdent <$> consume
    , TTypeIdent <$> consume
    ]

makePrisms ''Token

data LocToken =
  LocToken
    { tokenVal :: Token
    , tokenPos :: P.SourcePos
    , tokenSpellingDuring :: Text
    , tokenSpellingAfter :: Text
    }
  deriving (Eq, Ord)

locTokenPretty :: LocToken -> Text
locTokenPretty LocToken {..} =
  tokenPretty tokenVal
    <> " at "
    <> T.pack (P.sourcePosPretty tokenPos)
    <> " spelled "
    <> showT tokenSpellingDuring
    <> " with "
    <> showT tokenSpellingAfter

instance Show LocToken where
  show = T.unpack . locTokenPretty

recreateToken :: LocToken -> Text
recreateToken LocToken {..} = tokenSpellingDuring <> tokenSpellingAfter

recreateToken' :: Int -> LocToken -> Text
recreateToken' tw = replaceTabs tw . recreateToken

instance Lexable LocTokenState LocToken where
  consume = do
    tok <- consume
    consumeSpace
    sp <- use lexerPos
    sd <- use lexerSpellingDuring
    sa <- use lexerSpellingAfter
    return $ LocToken tok sp sd sa

instance P.Stream [LocToken] where
  type Token [LocToken] = LocToken
  type Tokens [LocToken] = [LocToken]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ []       = Nothing
  take1_ (t : ts) = Just (t, ts)
  takeN_ n s | n <= 0    = Just ([], s)
             | null s    = Nothing
             | otherwise = Just (splitAt n s)
  takeWhile_ = span
  showTokens Proxy =
    T.unpack . T.intercalate ", " . NE.toList . fmap (tokenPretty . tokenVal)
  reachOffset o P.PosState {..} =
    ( T.unpack line
    , P.PosState { P.pstateInput      = rst
                 , P.pstateOffset     = max pstateOffset o
                 , P.pstateSourcePos  = epos
                 , P.pstateTabWidth   = pstateTabWidth
                 , P.pstateLinePrefix = pstateLinePrefix
                 }
    )
   where
    ofDiff     = o - pstateOffset
    (tok, rst) = splitAt ofDiff pstateInput
    epos =
      updatePosString pstateSourcePos
        .   T.concat
        $   recreateToken' (P.unPos pstateTabWidth)
        <$> tok
    strs =
      T.splitOn "\n"
        .   T.concat
        $   recreateToken' (P.unPos pstateTabWidth)
        <$> pstateInput
    line = strs !! min (length strs - 1) ind
    ind  = ((-) `on` P.unPos . P.sourceLine) epos pstateSourcePos
