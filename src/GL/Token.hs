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
import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read               hiding ( Ident )
import           Control.Lens
import           GL.Token.Keyword
import           GL.Lexer.Lexable

updatePosString :: P.SourcePos -> String -> P.SourcePos
updatePosString p []          = p
updatePosString p ('\t' : xs) = updatePosString
  (p { P.sourceColumn = P.sourceColumn p <> P.defaultTabWidth })
  xs
updatePosString p ('\n' : xs) = updatePosString
  (p { P.sourceLine = P.sourceLine p <> P.pos1, P.sourceColumn = P.pos1 })
  xs
updatePosString p (_ : xs) =
  updatePosString (p { P.sourceColumn = P.sourceColumn p <> P.pos1 }) xs

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
  lexAP = foldl1
    (<++)
    [ do
      a <- lexAP
      b <- lift RP.look
      guard
        (null b || not (isLetter $ head $ fromKeyword a) || not
          (isAlphaNum $ head b)
        )
      return $ TKeyword a
    , TStringLit <$> lexAP
    , TIntLit <$> lexAP
    , TFloatLit <$> lexAP
    , TCharLit <$> lexAP
    , TIdent <$> lexAP
    , TTypeIdent <$> lexAP
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
