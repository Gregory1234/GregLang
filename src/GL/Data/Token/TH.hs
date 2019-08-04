{-# LANGUAGE TemplateHaskell #-}

module GL.Data.Token.TH where

import Data.Char
import Language.Haskell.TH

genKeywords :: [(String, String)] -> Q [Dec]
genKeywords xs = do
  let kw = mkName "Keyword"
  ys <-
    mapM
      (\(a, b) -> do
         let n = mkName ('K' : a)
         return (NormalC n []))
      xs
  ins <-
    mapM
      (\(a, b) -> do
         let n = mkName ('K' : a)
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

keywordNames :: [(String, String)]
keywordNames =
  map
    (\w ->
       let (x:xs) = words w
        in case xs of
             [] -> (x, map toLower x)
             (y:_) -> (x, y))
    [ "Class"
    , "If"
    , "Else"
    , "While"
    , "Do"
    , "For"
    , "Let"
    , "BraceOp {"
    , "BraceCl }"
    , "ParenOp ("
    , "ParenCl )"
    ]
