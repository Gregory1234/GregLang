{-# LANGUAGE Strict #-}

module GL.Lexer
  ( lexGregLang
  )
where

import           Data.Char
import           GL.Data.Token
import qualified Text.Megaparsec               as P
import           Text.Read
import           GL.Utils

spanSpace :: String -> (String, String)
spanSpace = span isSpace

lexGregLang :: FilePath -> String -> Either String [LocToken]
lexGregLang fn str =
  let (a, b) = spanSpace str
      p      = P.initialPos fn
  in  (LocToken TBegin p "" a :) <$> lexer b (updatePosString p a)

lexer :: String -> P.SourcePos -> Either String [LocToken]
lexer "" _ = Right []
lexer s  p = do
  ((ds, t), nds) <- listToEither ("couldnt lex (" ++ s ++ ")")
    $ readPrecGather s
  let (as, s') = spanSpace nds
  let p'       = updatePosString p (ds ++ as)
  ts <- lexer s' p'
  return $ LocToken t p ds as : ts
