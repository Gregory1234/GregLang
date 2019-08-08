{-# LANGUAGE RecordWildCards, FlexibleInstances, TypeFamilies,
  TemplateHaskell, Strict #-}

module GL.Data.Token where

import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.Functor
import           Data.List
import qualified Data.List.NonEmpty            as NE
import           Data.List.Split
import           Data.Proxy
import           GL.Data.TH
import           GL.Utils
import qualified Text.Megaparsec               as P
import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read

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

$(genEnum
    "Keyword"
    "K"
    (mkEnumList
       [ "Class"
       , "If"
       , "Else"
       , "While"
       , "Do"
       , "For"
       , "Let"
       , "Return"
       , "Import"
       , "Break"
       , "Continue"
       , "BraceOp {"
       , "BraceCl }"
       , "ParenOp ("
       , "ParenCl )"
       , "Semicolon ;"
       , "Equal =="
       , "LessEq <="
       , "GreaterEq >="
       , "NotEq !="
       , "AddSet +="
       , "SubSet -="
       , "MulSet *="
       , "DivSet /="
       , "ModSet %="
       , "AndSet &&="
       , "OrSet ||="
       , "XorSet ^^="
       , "BAndSet &="
       , "BOrSet |="
       , "BXorSet ^="
       , "Set ="
       , "Less <"
       , "Greater >"
       , "Not !"
       , "Add +"
       , "Sub -"
       , "Mul *"
       , "Div /"
       , "Mod %"
       , "And &&"
       , "Or ||"
       , "Xor ^^"
       , "BAnd &"
       , "BOr |"
       , "BXor ^"
       ]))

keywords :: [Keyword]
keywords = [minBound .. maxBound]

instance Read Keyword where
  readPrec = foldl1 (<++) $ map (\x -> lift (RP.string (show x)) $> x) keywords

data Token
  = TBegin
  | TIdent String
  | TTypeIdent String
  | TStringLit String
  | TIntLit Integer
  | TFloatLit Double
  | TCharLit Char
  | TKeyword Keyword
  deriving (Eq, Ord)

spellToken :: Token -> String
spellToken TBegin         = ""
spellToken (TIdent     x) = x
spellToken (TTypeIdent x) = x
spellToken (TStringLit x) = show x
spellToken (TIntLit    x) = show x
spellToken (TFloatLit  x) = show x
spellToken (TCharLit   x) = show x
spellToken (TKeyword   x) = show x

instance Show Token where
  show TBegin         = "<begin>"
  show (TIdent     s) = "<ident " ++ show s ++ ">"
  show (TTypeIdent s) = "<type ident " ++ show s ++ ">"
  show (TStringLit s) = "<string " ++ show s ++ ">"
  show (TIntLit    s) = "<int " ++ show s ++ ">"
  show (TFloatLit  s) = "<float " ++ show s ++ ">"
  show (TCharLit   s) = "<char " ++ show s ++ ">"
  show (TKeyword   s) = show (show s)

instance Read Token where
  readPrec = foldl1
    (<++)
    [ do
      a <- readPrec
      b <- lift RP.look
      guard
        (null b || not (isLetter $ head $ show a) || not (isAlphaNum $ head b))
      return $ TKeyword a
    , TStringLit <$> readPrec
    , TIntLit <$> readPrec
    , TFloatLit <$> readPrec
    , TCharLit <$> readPrec
    , TIdent <$> lift
      ((:) <$> RP.satisfy ((isAlpha &&& isLower) ||| (== '_')) <*> RP.munch
        (isAlphaNum ||| (== '_'))
      )
    , TTypeIdent <$> lift
      ((:) <$> RP.satisfy (isAlpha &&& isUpper) <*> RP.munch
        (isAlphaNum ||| (== '_'))
      )
    ]

data LocToken =
  LocToken
    { tokenVal :: Token
    , tokenPos :: P.SourcePos
    , tokenSpellingDuring :: String
    , tokenSpellingAfter :: String
    }
  deriving (Eq, Ord)

instance Show LocToken where
  show LocToken {..} =
    show tokenVal ++ " at " ++ P.sourcePosPretty tokenPos ++ " spelled " ++ show
      (tokenSpellingDuring ++ tokenSpellingAfter)

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
  showTokens Proxy = intercalate ", " . NE.toList . fmap (show . tokenVal)
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
