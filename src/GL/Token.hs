{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module GL.Token
  ( module GL.Token
  , module GL.Token.Keyword
  , module GL.Loc
  )
where

import           Control.Lens
import           Control.Monad
import           Data.Char
import qualified Data.List.NonEmpty            as NE
import           Data.Proxy
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as P

import           GL.Lexer
import           GL.Loc
import           GL.Token.Keyword
import           GL.Utils

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

makePrisms ''Token

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

instance Lexable Token where
  consume = asum
    [ (uses lexTokId (== 0) >>= guard) $> TBegin
    , do
      a <- consume
      b <- use lexRest
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

locTokenPretty :: LocT Token -> Text
locTokenPretty LocT {..} = tokenPretty _tokVal <> " at " <> locPretty _tokLoc

instance Show (LocT Token) where
  show = T.unpack . locTokenPretty

instance P.Stream [LocT Token] where
  type Token [LocT Token] = LocT Token
  type Tokens [LocT Token] = [LocT Token]
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
    T.unpack . T.intercalate ", " . NE.toList . fmap (tokenPretty . _tokVal)
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
      (pstateSourcePos <++)
        .   T.concat
        $   recreateToken' (P.unPos pstateTabWidth)
        .   _tokLoc
        <$> tok
    strs =
      T.splitOn "\n"
        .   T.concat
        $   recreateToken' (P.unPos pstateTabWidth)
        .   _tokLoc
        <$> pstateInput
    line = strs !! min (length strs - 1) ind
    ind  = ((-) `on` P.unPos . P.sourceLine) epos pstateSourcePos
