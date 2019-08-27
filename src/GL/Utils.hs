{-# LANGUAGE FlexibleInstances, DefaultSignatures,
  GeneralizedNewtypeDeriving #-}

module GL.Utils
  ( module Data.Tree
  , module Data.Bifunctor
  , on
  , ($>)
  , void
  , join
  , module Data.Default.Class
  , module Data.Functor.Identity
  , module GL.Utils
  )
where

import qualified Data.List.HT                  as L
import           Data.Tree
import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read
import           Data.List
import           Data.Bifunctor
import           Data.Function
import           Data.Functor                   ( ($>) )
import           Data.Maybe.HT
import           Control.Monad
import           Data.Default.Class
import           Control.Lens
import           Data.Functor.Identity

class Treeable a where
  toTree :: a -> Tree String

listToTree :: Treeable a => String -> [a] -> Tree String
listToTree s = Node s . map toTree

toForest :: Treeable a => [a] -> Forest String
toForest = map toTree

instance Treeable String where
  toTree s = Node s []

instance Treeable (Tree String) where
  toTree = id

treePP :: Treeable a => a -> String
treePP = drawTree . toTree

replaceTabs :: Int -> String -> String
replaceTabs tw = L.replace "\t" (replicate tw ' ')

appDb :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
appDb f g h x = f (g x) (h x)

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = appDb (||)

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = appDb (&&)

lexGather :: Lexable a => ReadS (String, a)
lexGather = RP.readP_to_S (RP.gather (readPrec_to_P lexAP 0))

listToEither :: b -> [a] -> Either b a
listToEither b []      = Left b
listToEither _ (x : _) = Right x

breakList :: Eq a => [a] -> [a] -> ([a], [a])
breakList xs ys | xs `isPrefixOf` ys = ([], ys)
breakList _  []                      = ([], [])
breakList xs (y : ys)                = first (y :) $ breakList xs ys

infixl 4 <&>
(<&>) :: Applicative f => f a -> f b -> f (a, b)
a <&> b = (,) <$> a <*> b

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

lexElem :: (Pretty a, Pretty b, Lexable b) => [b] -> a -> Maybe b
lexElem l a = toMaybe (showPP a `elem` map showPP l) (lexS $ showPP a)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither x Nothing  = Left x

headEither :: a -> [b] -> Either a b
headEither x []      = Left x
headEither _ (x : _) = Right x

onlyEither :: a -> [b] -> Either a b
onlyEither _ [x] = Right x
onlyEither x _   = Left x

eitherConcat :: Monoid m => [Either a m] -> Either a m
eitherConcat (Left  x : _ ) = Left x
eitherConcat (Right x : xs) = case eitherConcat xs of
  Left  a -> Left a
  Right a -> Right (a <> x)
eitherConcat [] = Right mempty

setAt :: (Integral n, Default a) => n -> a -> [a] -> [a]
setAt n _ _ | n < 0 = error "GL.Utils.setAt: negative index"
setAt 0 a []        = [a]
setAt n a []        = def : setAt (n - 1) a []
setAt 0 a (_ : xs)  = a : xs
setAt n a (x : xs)  = x : setAt (n - 1) a xs

getAt :: (Integral n, Default a) => n -> [a] -> a
getAt n _ | n < 0 = error "GL.Utils.getAt: negative index"
getAt _ []        = def
getAt 0 (x : _ )  = x
getAt n (_ : xs)  = getAt (n - 1) xs

tryTillStableM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
tryTillStableM f a = do
  b <- f a
  if b == a then return a else tryTillStableM f b

infixl 4 *>>=
(*>>=) :: (Monad m) => m (a -> m b) -> m a -> m b
(*>>=) f a = join $ f <*> a

class Pretty a where
  showPP :: a -> String
  default showPP :: Show a => a -> String
  showPP = show

instance Pretty Char
instance Pretty String
instance Pretty Int
instance Pretty Integer
instance Pretty Float
instance Pretty Double

newtype ClearString = ClearString {getClearString :: String}

instance Pretty ClearString where
  showPP = getClearString

newtype PrettyTree t =
  PrettyTree t
  deriving Treeable

instance Treeable t => Pretty (PrettyTree t) where
  showPP = treePP

pprint :: Pretty a => a -> IO ()
pprint = putStrLn . showPP

class Lexable a where
  lexAP :: ReadPrec a
  default lexAP :: Read a => ReadPrec a
  lexAP = readPrec

lexA :: Lexable a => String -> [(a, String)]
lexA = readPrec_to_S lexAP 0

lexS :: Lexable a => String -> a
lexS = fst . head . lexA

instance Lexable Int
instance Lexable Integer
instance Lexable Float
instance Lexable Double
instance Lexable String
instance Lexable Char

_Pretty :: (Lexable a, Pretty a) => Prism' String a
_Pretty = prism showPP $ \s -> second fst $ headEither s $ lexA s
