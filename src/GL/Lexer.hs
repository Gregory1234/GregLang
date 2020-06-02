{-# LANGUAGE Strict #-}

module GL.Lexer
  ( module GL.Lexer
  , module GL.Lexer.Lexable
  , module GL.Token
  )
where

import           GL.Token
import qualified Text.Megaparsec               as P
import           GL.Lexer.Lexable
import           GL.Utils
import           Data.Char
import           Data.Bool

data WhitespaceType = Whitespace | LineComment | BlockComment

spanSpace :: WhitespaceType -> String -> (String, String)
spanSpace Whitespace ('/' : '/' : xs) =
  let (w, r) = spanSpace LineComment xs in ('/' : '/' : w, r)
spanSpace Whitespace ('/' : '*' : xs) =
  let (w, r) = spanSpace BlockComment xs in ('/' : '*' : w, r)
spanSpace Whitespace (x : xs) | isSpace x =
  let (w, r) = spanSpace Whitespace xs in (x : w, r)
spanSpace Whitespace xs = ("", xs)
spanSpace LineComment ('\n' : xs) =
  let (w, r) = spanSpace Whitespace xs in ('\n' : w, r)
spanSpace BlockComment ('*' : '/' : xs) =
  let (w, r) = spanSpace Whitespace xs in ('*' : '/' : w, r)
spanSpace t (x : xs) = let (w, r) = spanSpace t xs in (x : w, r)
spanSpace _ []       = ("", "")



lexGregLang :: FilePath -> String -> Either String [LocToken]
lexGregLang fn str = let pos = P.initialPos fn in helper False pos str
 where
  helper True _   [] = Right []
  helper b    pos s  = do
    ((spell, stuff), rest) <- bool
      (const . Right $ (("", TBegin), str))
      (headError ("couldn't lex " ++ show s) . lexGather)
      b
      s
    let (whspc, rest') = spanSpace Whitespace rest
    let pos'           = updatePosString pos (spell ++ whspc)
    (LocToken stuff pos spell whspc :) <$> helper True pos' rest'
