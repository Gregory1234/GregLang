{-# LANGUAGE Strict #-}

module GL.Lexer
  ( lexGregLang
  ) where

import Data.Char
import GL.Data.Token
import qualified Text.Megaparsec as P
import Text.Read

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

splitGreedyRead :: String -> Maybe [(Token, String)]
splitGreedyRead "" = Just []
splitGreedyRead s =
  let l = reads s
   in case l of
        [] -> Nothing
        ((a, r):_) ->
          ((a, take (length $ spellToken a) s) :) <$> splitGreedyRead r

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
lexer ('\'':s) p = do
  let (ds, nds) = helper s ""
  let (as, s') = span isSpace nds
  w <- maybeToEither ('\'' : ds) $ readMaybe ('\'' : ds)
  let p' = updatePosString p (('\'' : ds) ++ as)
  xs <- lexer s' p'
  return (LocToken w p ('\'' : ds) as : xs)
  where
    helper xs ys =
      let (ds, nds) = break (\x -> x == '\'' || x == '\\') xs
          (as, d:ds') = span (== '\\') nds
       in if even (length as) && d == '\''
            then (ys ++ ds ++ as ++ "\'", ds')
            else helper ds' (ys ++ ds ++ as ++ pure d)
lexer ('"':s) p = do
  let (ds, nds) = helper s ""
  let (as, s') = span isSpace nds
  w <- maybeToEither ('"' : ds) $ readMaybe ('"' : ds)
  let p' = updatePosString p (('"' : ds) ++ as)
  xs <- lexer s' p'
  return (LocToken w p ('"' : ds) as : xs)
  where
    helper xs ys =
      let (ds, nds) = break (\x -> x == '"' || x == '\\') xs
          (as, d:ds') = span (== '\\') nds
       in if even (length as) && d == '"'
            then (ys ++ ds ++ as ++ "\"", ds')
            else helper ds' (ys ++ ds ++ as ++ pure d)
lexer s p = do
  let (ds, nds) = break isSpace s
  let (as, s') = span isSpace nds
  ws <-
    maybeToEither ds $
    if ds == ""
      then Just [(TBegin, "")]
      else splitGreedyRead ds
  let p' = updatePosString p (ds ++ as)
  xs <- lexer s' p'
  let ws' = helper ws p as
  return (ws' ++ xs)
  where
    helper [] _ _ = []
    helper [(w, ds)] p as = [LocToken w p ds as]
    helper ((w, ds):ws) p as =
      LocToken w p ds "" : helper ws (updatePosString p ds) as
