{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module GL.Token.TH
  ( module GL.Token.TH
  )
where

import           Language.Haskell.TH
import           Data.Bifunctor

keywordType :: String -> [(String, String)] -> Q [Dec]
keywordType name' (map (first mkName) -> vals) = pure
  [dataDecl, fromSig, fromDecl, toSig, toDecl]
 where
  name     = mkName name'
  from     = mkName ("from" ++ name')
  to       = mkName ("to" ++ name')

  arrT     = AppT . AppT ArrowT
  strT     = AppT ListT (ConT ''Char)

  dataDecl = DataD [] name [] Nothing constructors deriveClauses
   where
    constructors  = map (\(x, _) -> NormalC x []) vals
    derivs        = [''Eq, ''Ord, ''Enum, ''Bounded, ''Show, ''Read]
    deriveClauses = [DerivClause Nothing (map ConT derivs)]

  fromSig = SigD from $ arrT (ConT name) strT
  fromHelper (n, s) = Clause [ConP n []] (NormalB (LitE s)) []
  fromDecl = FunD from (map (fromHelper . second StringL) vals)

  toSig    = SigD to $ arrT strT (ConT name)
  toHelper (n, s) = Clause [LitP s] (NormalB (ConE n)) []
  toDecl = FunD to (map (toHelper . second StringL) vals)
