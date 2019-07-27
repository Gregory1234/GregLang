{-# LANGUAGE TupleSections #-}
module GL.Lexer(
  lexGregLang
) where

import Data.Char
import Text.Read
import Control.Applicative

import qualified Text.Megaparsec as P

import GL.Data.Token

updatePosString :: P.SourcePos -> String -> P.SourcePos
updatePosString p s = let (x,y,z) = P.reachOffset (length s) (P.PosState s 0 p P.defaultTabWidth "") in x

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

splitGreedyRead :: Read a => String -> Maybe [(a,String)]
splitGreedyRead "" = Just []
splitGreedyRead s = helper s ""
  where
    helper "" _ = Nothing
    helper s1 s2 = maybe
      (helper (init s1) (last s1:s2))
      (\a -> fmap ((a,s1):) $ splitGreedyRead s2)
      (readMaybe s1)

lexGregLang :: FilePath -> String -> Either String [LocToken]
lexGregLang fn str = lexer str (P.initialPos fn)

lexer :: String -> P.SourcePos -> Either String [LocToken]
lexer "" _ = Right []
lexer s p = do
  let (ds,nds) = span (not . isSpace) s
  let (as,s') = span isSpace nds
  ws <- maybeToEither (show ds) $ if ds == "" then Just [(TBegin,"")] else splitGreedyRead ds
  let p' = updatePosString p (ds++as)
  xs <- lexer s' p'
  let ws' = helper ws p as
  return (ws'++xs)
    where
      helper ((w,s):[]) p as = [LocToken w p s as]
      helper ((w,s):ws) p as = LocToken w p s "":helper ws (updatePosString p s) as
