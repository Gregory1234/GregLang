module GL.Lexer
  ( lexGregLang
  ) where

import Control.Applicative
import Data.Char
import GL.Data.Token
import qualified Text.Megaparsec as P
import Text.Read

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

splitGreedyRead :: Read a => String -> Maybe [(a, String)]
splitGreedyRead "" = Just []
splitGreedyRead s = helper s ""
  where
    helper "" _ = Nothing
    helper s1 s2 =
      maybe
        (helper (init s1) (last s1 : s2))
        (\a -> ((a, s1) :) <$> splitGreedyRead s2)
        (readMaybe s1)

lexGregLang :: FilePath -> String -> Either String [LocToken]
lexGregLang fn str = lexer' str (P.initialPos fn)

lexer' :: String -> P.SourcePos -> Either String [LocToken]
lexer' s p =
  case lexer s p of
    Left e -> Left e
    Right [] -> Right [beg]
    Right a@(LocToken TBegin _ _ _:_) -> Right a
    Right a@(_:_) -> Right (beg : a)
  where
    beg = LocToken TBegin p "" ""

lexer :: String -> P.SourcePos -> Either String [LocToken]
lexer "" _ = Right []
lexer s p = do
  let (ds, nds) = break isSpace s
  let (as, s') = span isSpace nds
  ws <-
    maybeToEither (show ds) $
    if ds == ""
      then Just [(TBegin, "")]
      else splitGreedyRead ds
  let p' = updatePosString p (ds ++ as)
  xs <- lexer s' p'
  let ws' = helper ws p as
  return (ws' ++ xs)
  where
    helper [(w, s)] p as = [LocToken w p s as]
    helper ((w, s):ws) p as =
      LocToken w p s "" : helper ws (updatePosString p s) as
