{-# LANGUAGE Strict #-}

module GL.Lexer
  ( lexGregLang
  , Lexable(..)
  , module GL.Token
  )
where

import           Data.Char
import           GL.Token
import qualified Text.Megaparsec               as P
import           GL.Utils
import           GL.Lexer.Lexable

spanSpace :: String -> (String, String)
spanSpace xs@('\\' : '\\' : _) =
  let (a, b) = break (== '\n') xs in first (a ++) (spanSpace b)
spanSpace xs@('\\' : '*' : _) =
  let (a, _ : _ : b) = breakList "*\\" xs
  in  first ((a ++ "*\\") ++) (spanSpace b)
spanSpace xs@(x : _) | isSpace x =
  let (a, b) = span isSpace xs in first (a ++) (spanSpace b)
spanSpace xs = ([], xs)

lexGregLang :: FilePath -> String -> Either String [LocToken]
lexGregLang fn str =
  let (a, b) = spanSpace str
      p      = P.initialPos fn
  in  (LocToken TBegin p "" a :) <$> lexer b (updatePosString p a)

lexer :: String -> P.SourcePos -> Either String [LocToken]
lexer "" _ = Right []
lexer s  p = do
  ((ds, t), nds) <- headError ("couldnt lex (" ++ s ++ ")") $ lexGather s
  let (as, s') = spanSpace nds
  let p'       = updatePosString p (ds ++ as)
  ts <- lexer s' p'
  return $ LocToken t p ds as : ts
