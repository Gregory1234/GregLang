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
import           Data.List
import qualified Data.List.NonEmpty            as NE
import           Data.List.Split
import           Data.Proxy
import           GL.Utils
import qualified Text.Megaparsec               as P
import           Control.Lens
import           GL.Token.Keyword
import           GL.Lexer

data Token
  = TBegin
  | TIdent Ident
  | TTypeIdent ClassName
  | TStringLit String
  | TIntLit Integer
  | TFloatLit Double
  | TCharLit Char
  | TKeyword Keyword
  deriving (Eq, Ord, Show)

spellToken :: Token -> String
spellToken TBegin         = ""
spellToken (TIdent     x) = getIdent x
spellToken (TTypeIdent x) = getClassName x
spellToken (TStringLit x) = show x
spellToken (TIntLit    x) = show x
spellToken (TFloatLit  x) = show x
spellToken (TCharLit   x) = show x
spellToken (TKeyword   x) = fromKeyword x

tokenPretty :: Token -> String
tokenPretty TBegin         = "<begin>"
tokenPretty (TIdent     s) = "<ident " ++ getIdent s ++ ">"
tokenPretty (TTypeIdent s) = "<type ident " ++ getClassName s ++ ">"
tokenPretty (TStringLit s) = "<string " ++ show s ++ ">"
tokenPretty (TIntLit    s) = "<int " ++ show s ++ ">"
tokenPretty (TFloatLit  s) = "<float " ++ show s ++ ">"
tokenPretty (TCharLit   s) = "<char " ++ show s ++ ">"
tokenPretty (TKeyword   s) = show (fromKeyword s)

instance Lexable Token where
  consume = asum
    [ (uses hadBegin not >>= guard >> hadBegin .= True) $> TBegin
    , do
      a <- consume
      b <- use lexerData
      guard
        (null b || not (isLetter $ head $ fromKeyword a) || not
          (isAlphaNum $ head b)
        )
      return $ TKeyword a
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
    , tokenSpellingDuring :: String
    , tokenSpellingAfter :: String
    }
  deriving (Eq, Ord)

locTokenPretty :: LocToken -> String
locTokenPretty LocToken {..} =
  tokenPretty tokenVal
    ++ " at "
    ++ P.sourcePosPretty tokenPos
    ++ " spelled "
    ++ show tokenSpellingDuring
    ++ " with "
    ++ show tokenSpellingAfter

instance Show LocToken where
  show = locTokenPretty

recreateToken :: LocToken -> String
recreateToken LocToken {..} = tokenSpellingDuring ++ tokenSpellingAfter

recreateToken' :: Int -> LocToken -> String
recreateToken' tw = replaceTabs tw . recreateToken

instance Lexable LocToken where
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
    intercalate ", " . NE.toList . fmap (tokenPretty . tokenVal)
  reachOffset o P.PosState {..} =
    ( line
    , P.PosState { P.pstateInput      = rest
                 , P.pstateOffset     = max pstateOffset o
                 , P.pstateSourcePos  = epos
                 , P.pstateTabWidth   = pstateTabWidth
                 , P.pstateLinePrefix = pstateLinePrefix
                 }
    )
   where
    ofDiff      = o - pstateOffset
    (tok, rest) = splitAt ofDiff pstateInput
    epos        = updatePosString pstateSourcePos $ tok >>= recreateToken'
      (P.unPos pstateTabWidth)
    strs =
      splitOn "\n" (pstateInput >>= recreateToken' (P.unPos pstateTabWidth))
    line = strs !! min (length strs - 1) ind
    ind  = ((-) `on` P.unPos . P.sourceLine) epos pstateSourcePos
