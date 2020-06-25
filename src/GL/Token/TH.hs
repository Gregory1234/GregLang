{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module GL.Token.TH
  ( module GL.Token.TH
  )
where

import           Language.Haskell.TH
import           Data.Bifunctor

keywordType :: String -> [(String, String)] -> Q [Dec]
keywordType name' (map (bimap mkName StringL) -> vals) = pure
  [dataDecl, fromSig, fromDecl, toSig, toDecl]
 where
  name     = mkName name'
  from     = mkName ("from" ++ name')
  to       = mkName ("to" ++ name')
  derivs   = [''Eq, ''Ord, ''Enum, ''Bounded, ''Show, ''Read]
  dataDecl = DataD [] name [] Nothing constructors deriveClauses
   where
    constructors  = map (\(x, _) -> NormalC x []) vals
    deriveClauses = [DerivClause Nothing (map ConT derivs)]
  arrHelper = AppT . AppT ArrowT
  fromSig   = SigD from $ arrHelper (ConT name) (AppT ListT (ConT ''Char))
  fromHelper (n, s) = Clause [ConP n []] (NormalB (LitE s)) []
  fromDecl = FunD from (map fromHelper vals)
  toSig    = SigD to $ arrHelper (AppT ListT (ConT ''Char)) (ConT name)
  toHelper (n, s) = Clause [LitP s] (NormalB (ConE n)) []
  toDecl = FunD to (map toHelper vals)
