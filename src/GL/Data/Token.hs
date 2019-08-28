{-# LANGUAGE TemplateHaskell, RecordWildCards, FlexibleInstances, TypeFamilies,
   Strict, DerivingVia, GeneralizedNewtypeDeriving #-}

module GL.Data.Token
  ( module GL.Data.Token
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
import           GL.Data.Ident
import           Data.String
import           Control.Lens

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

newtype Keyword = Keyword { unKeyword :: String }
  deriving newtype (Eq, Ord, IsString)
  deriving Pretty via ClearString

keywords :: [String]
keywords = concat
  [ ["class", "package", "import", "if", "else", "while", "do"]
  , ["for", "let", "return", "import", "break", "continue"]
  , ["{", "}", "(", ")", "[", "]", ";", ".", ","]
  , ["==", "<=", ">=", "!=", "+=", "-=", "*=", "/=", "%="]
  , ["&&=", "||=", "^^=", "&=", "|=", "^="]
  , ["=", "<", ">", "!", "+", "-", "*", "/", "%"]
  , ["&&", "||", "^^", "&", "|", "^"]
  ]

instance Lexable Keyword where
  lexAP = Keyword <$> foldl1 (<++) (map (lift . RP.string) keywords)

instance Enum Keyword where
  toEnum   = Keyword . (keywords !!)
  fromEnum = fromJust . (`elemIndex` keywords) . unKeyword

instance Bounded Keyword where
  minBound = Keyword (head keywords)
  maxBound = Keyword (last keywords)

data Token
  = TBegin
  | TIdent Ident
  | TTypeIdent ClassName
  | TStringLit String
  | TIntLit Integer
  | TFloatLit Double
  | TCharLit Char
  | TKeyword Keyword
  deriving (Eq, Ord)

spellToken :: Token -> String
spellToken TBegin         = ""
spellToken (TIdent     x) = showPP x
spellToken (TTypeIdent x) = showPP x
spellToken (TStringLit x) = showPP x
spellToken (TIntLit    x) = showPP x
spellToken (TFloatLit  x) = showPP x
spellToken (TCharLit   x) = showPP x
spellToken (TKeyword   x) = showPP x

instance Pretty Token where
  showPP TBegin         = "<begin>"
  showPP (TIdent     s) = "<ident " ++ showPP s ++ ">"
  showPP (TTypeIdent s) = "<type ident " ++ showPP s ++ ">"
  showPP (TStringLit s) = "<string " ++ showPP s ++ ">"
  showPP (TIntLit    s) = "<int " ++ showPP s ++ ">"
  showPP (TFloatLit  s) = "<float " ++ showPP s ++ ">"
  showPP (TCharLit   s) = "<char " ++ showPP s ++ ">"
  showPP (TKeyword   s) = showPP (showPP s)

instance Lexable Token where
  lexAP = foldl1
    (<++)
    [ do
      a <- lexAP
      b <- lift RP.look
      guard
        (null b || not (isLetter $ head $ showPP a) || not (isAlphaNum $ head b)
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
    { _tokenVal :: Token
    , _tokenPos :: P.SourcePos
    , tokenSpellingDuring :: String
    , tokenSpellingAfter :: String
    }
  deriving (Eq, Ord)

instance Pretty LocToken where
  showPP LocToken {..} =
    showPP _tokenVal
      ++ " at "
      ++ P.sourcePosPretty _tokenPos
      ++ " spelled "
      ++ showPP (tokenSpellingDuring ++ tokenSpellingAfter)

instance Pretty [LocToken] where
  showPP = unlines . map showPP

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
  showTokens Proxy = intercalate ", " . NE.toList . fmap (showPP . _tokenVal)
  reachOffset o P.PosState {..} =
    ( epos
    , line
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


makeLenses ''LocToken
