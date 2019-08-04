{-# LANGUAGE TemplateHaskell #-}

module GL.Data.TH where

import Data.Char
import Language.Haskell.TH

genEnum :: String -> String -> [(String, String)] -> Q [Dec]
genEnum n pf xs = do
  let kw = mkName n
  ys <-
    mapM
      (\(a, b) -> do
         let n = mkName (pf ++ a)
         return (NormalC n []))
      xs
  ins <-
    mapM
      (\(a, b) -> do
         let n = mkName (pf ++ a)
         return (Clause [ConP n []] (NormalB (LitE (StringL b))) []))
      xs
  return
    [ DataD
        []
        kw
        []
        Nothing
        ys
        [ DerivClause
            Nothing
            [ConT ''Eq, ConT ''Ord, ConT ''Enum, ConT ''Bounded]
        ]
    , InstanceD
        Nothing
        []
        (AppT (ConT ''Show) (ConT kw))
        [FunD (mkName "show") ins]
    ]

mkEnumList :: [String] -> [(String, String)]
mkEnumList =
  map
    (\w ->
       let (x:xs) = words w
        in case xs of
             [] -> (x, map toLower x)
             (y:_) -> (x, y))
