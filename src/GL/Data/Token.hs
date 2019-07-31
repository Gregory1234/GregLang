{-# LANGUAGE RecordWildCards, FlexibleInstances, TypeFamilies #-}

module GL.Data.Token where

import Control.Monad
import Data.Char
import Data.Function
import Data.Functor
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import Data.Proxy
import GL.Utils
import qualified Text.Megaparsec as P
import qualified Text.ParserCombinators.ReadP as RP
import Text.Read

updatePosString :: P.SourcePos -> String -> P.SourcePos
updatePosString p s =
  let (x, y, z) =
        P.reachOffset (length s) (P.PosState s 0 p P.defaultTabWidth "")
   in x

data Token
  = TBegin
  | TIdent String
  | TStringLit String
  | TIntLit Integer
  | TFloatLit Double
  | TCharLit Char
  | TKeyword Keyword
  deriving (Eq, Ord)

spellToken :: Token -> String
spellToken TBegin = ""
spellToken (TIdent x) = x
spellToken (TStringLit x) = show x
spellToken (TIntLit x) = show x
spellToken (TFloatLit x) = show x
spellToken (TCharLit x) = show x
spellToken (TKeyword x) = show x

instance Show Token where
  show TBegin = "<begin>"
  show (TIdent s) = "<ident " ++ show s ++ ">"
  show (TStringLit s) = "<string " ++ show s ++ ">"
  show (TIntLit s) = "<int " ++ show s ++ ">"
  show (TFloatLit s) = "<float " ++ show s ++ ">"
  show (TCharLit s) = "<char " ++ show s ++ ">"
  show (TKeyword s) =
    let x = show s
     in if isAlphaNum (head x)
          then show x
          else "'" ++ x ++ "'"

instance Read Token where
  readPrec =
    foldl1
      (<++)
      [ TKeyword <$> readPrec
      , TStringLit <$> readPrec
      , TIntLit <$> readPrec
      , TFloatLit <$> readPrec
      , TCharLit <$> readPrec
      , TIdent <$>
        lift
          ((:) <$> RP.satisfy (\x -> isAlpha x || x == '_') <*>
           RP.munch (\x -> isAlphaNum x || x == '_'))
      ]

data Keyword
  = KClass
  | KIf
  | KElse
  | KFor
  | KWhile
  | KLet
  | KBrackOp
  | KBrackCl
  | KBraceOp
  | KBraceCl
  | KParenOp
  | KParenCl
  deriving (Eq, Ord, Enum, Bounded)

instance Show Keyword where
  show KClass = "class"
  show KIf = "if"
  show KElse = "else"
  show KFor = "for"
  show KWhile = "while"
  show KLet = "let"
  show KBrackOp = "["
  show KBrackCl = "]"
  show KBraceOp = "{"
  show KBraceCl = "}"
  show KParenOp = "("
  show KParenCl = ")"

instance Read Keyword where
  readPrec =
    foldl1 (<++) $
    map (\x -> lift (RP.string $ show x) $> x) [minBound .. maxBound]

data LocToken =
  LocToken
    { tokenVal :: Token
    , tokenPos :: P.SourcePos
    , tokenSpellingDuring :: String
    , tokenSpellingAfter :: String
    }
  deriving (Eq, Ord)

instance Show LocToken where
  show LocToken {..} = show tokenVal ++ " at " ++ P.sourcePosPretty tokenPos

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
  take1_ [] = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0 = Just ([], s)
    | null s = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span
  showTokens Proxy = intercalate ", " . NE.toList . fmap (show . tokenVal)
  reachOffset o P.PosState {..} =
    ( epos
    , line
    , P.PosState
        { P.pstateInput = rest
        , P.pstateOffset = max pstateOffset o
        , P.pstateSourcePos = epos
        , P.pstateTabWidth = pstateTabWidth
        , P.pstateLinePrefix = pstateLinePrefix
        })
    where
      ofDiff = o - pstateOffset
      (tok, rest) = splitAt ofDiff pstateInput
      epos =
        updatePosString pstateSourcePos $
        tok >>= recreateToken' (P.unPos pstateTabWidth)
      strs =
        splitOn "\n" (pstateInput >>= recreateToken' (P.unPos pstateTabWidth))
      line = strs !! min (length strs - 1) ind
      ind = ((-) `on` P.unPos . P.sourceLine) epos pstateSourcePos
