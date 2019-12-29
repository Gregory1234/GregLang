{-# LANGUAGE FlexibleInstances, DefaultSignatures,
  GeneralizedNewtypeDeriving #-}

module GL.Utils
  ( module Data.Tree
  , module Data.Bifunctor
  , on
  , ($>)
  , void
  , join
  , zipWithM
  , module Data.Functor.Identity
  , module Data.Maybe
  , module Data.Foldable
  , toMaybe
  , liftA2
  , (<|>)
  , module Debug.Trace
  , module GL.Utils
  )
where

import qualified Data.List.HT                  as L
import           Data.Tree
import qualified Text.ParserCombinators.ReadP  as RP
import           Text.Read               hiding ( Lexeme(..) )
import           Data.List
import           Data.Bifunctor
import           Data.Function
import           Data.Functor                   ( ($>) )
import           Data.Maybe.HT
import           Data.Maybe
import           Control.Monad
import           Control.Lens
import           Data.Functor.Identity
import           Control.Monad.Except
import           Data.Foldable
import           Control.Applicative
import           Debug.Trace


-- | Like 'trace' but returns both the shown value and a third value.
--
-- >>> traceN "msg " 12
-- msg 12
-- 12
traceN
  :: Show a
  => String -- ^ the prepended message
  -> a
  -> a
traceN s a = trace (s ++ show a) a

-- | Like 'traceN' but using Pretty.
--
-- >>> traceP "msg " 12
-- msg 12
-- 12
traceP
  :: Pretty a
  => String -- ^ the prepended message
  -> a
  -> a
traceP s a = trace (s ++ showPP a) a

-- | Conversion of values to 'Tree' of 'String's.
class Treeable a where
  {-# MINIMAL toTree #-}
  -- | Convert a value to 'Tree' of 'String's.
  toTree :: a -> Tree String

-- | Convert a list of values to 'Tree' of 'String's with a given root.
listToTree
  :: Treeable a
  => String -- ^ Root of the tree
  -> [a]
  -> Tree String
listToTree s = Node s . map toTree

-- | Convert a list of values to 'Forest' of 'String's.
--
-- > toForest = map toTree
toForest :: Treeable a => [a] -> Forest String
toForest = map toTree

instance Treeable String where
  toTree s = Node s []

instance Treeable (Tree String) where
  toTree = id

-- | Pretty printing of a tree.
treePP :: Treeable a => a -> String
treePP = drawTree . toTree

-- | Replaces tabs in a 'String' with a given amount of spaces using 'L.replace'.
replaceTabs
  :: Int -- ^ Anount of spaces
  -> String
  -> String
replaceTabs tw = L.replace "\t" (replicate tw ' ')

-- | 'liftA2' on '||'
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = liftA2 (||)

-- | 'liftA2' on '&&'
(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = liftA2 (&&)

-- | Read both the value and the used up 'String'.
lexGather :: Lexable a => ReadS (String, a)
lexGather = RP.readP_to_S (RP.gather (readPrec_to_P lexAP 0))

-- | Split a list on another list.
--
-- >>> breakList "wor" "helloworld"
-- ("hello", "world")
--
-- >>> breakList "abc" "helloworld"
-- ("helloworld", "")
breakList :: Eq a => [a] -> [a] -> ([a], [a])
breakList xs ys | xs `isPrefixOf` ys = ([], ys)
breakList _  []                      = ([], [])
breakList xs (y : ys)                = first (y :) $ breakList xs ys

infixl 4 <&>
-- | 'liftA2' on the pair constructor.
(<&>) :: Applicative f => f a -> f b -> f (a, b)
(<&>) = liftA2 (,)

-- | List all elements of a 'Bounded' 'Enum'.
enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

lexElem :: (Pretty a, Pretty b, Lexable b) => [b] -> a -> Maybe b
lexElem l a = toMaybe (showPP a `elem` map showPP l) (lexS $ showPP a)

-- | Change a 'Maybe' into an 'Either'.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just x) = Right x
maybeToEither x Nothing  = Left x

-- | Safe version of 'head'.
headError :: MonadError a m => a -> [b] -> m b
headError x []      = throwError x
headError _ (x : _) = return x

-- | Safe version of 'only'.
onlyEither :: a -> [b] -> Either a b
onlyEither _ [x] = Right x
onlyEither x _   = Left x

joinFun :: (Monad m) => m (a -> m b) -> a -> m b
joinFun f a = ($ a) =<< f

-- | Conversion of values to formatted 'String's.
class Pretty a where
  -- | Convert a value to a formatted 'String's.
  showPP :: a -> String
  default showPP :: Show a => a -> String
  showPP = show

instance Pretty Char
instance Pretty String
instance Pretty Int
instance Pretty Integer
instance Pretty Float
instance Pretty Double

-- | Newtype for pretty printing 'String's without quotes.
newtype ClearString = ClearString {getClearString :: String}

instance Pretty ClearString where
  showPP = getClearString

-- | Newtype for pretty printing 'Treeable's.
newtype PrettyTree t =
  PrettyTree t
  deriving Treeable

instance Treeable t => Pretty (PrettyTree t) where
  showPP = treePP

-- | 'print' for 'Pretty'.
pprint :: Pretty a => a -> IO ()
pprint = putStrLn . showPP

-- | 'showPP' for 'Foldable's of 'Pretty'.
showPPList :: (Foldable f, Pretty a) => f a -> String
showPPList = foldr helper "[]"
 where
  helper a "[]"       = '[' : showPP a ++ "]"
  helper a ('[' : xs) = '[' : showPP a ++ ',' : xs
  helper _ _          = error "impossible"
-- Parsing of 'String's, producing values.
class Lexable a where
  -- | 'readPrec' for 'Lexable'
  lexAP :: ReadPrec a
  default lexAP :: Read a => ReadPrec a
  lexAP = readPrec

-- | 'reads' for 'Lexable'
lexA :: Lexable a => String -> [(a, String)]
lexA = readPrec_to_S lexAP 0

-- | 'read' for 'Lexable'
lexS :: Lexable a => String -> a
lexS = fst . head . lexA

instance Lexable Int
instance Lexable Integer
instance Lexable Float
instance Lexable Double
instance Lexable String
instance Lexable Char

-- | A 'Prism'' for text formatting based on 'Lexable' and 'Pretty'.
_Pretty :: (Lexable a, Pretty a) => Prism' String a
_Pretty = prism showPP $ \s -> second fst $ headError s $ lexA s

foldMapA :: (Applicative f, Monoid m, Foldable t) => (a -> f m) -> t a -> f m
foldMapA f = foldr (liftA2 mappend . f) (pure mempty)

-- | Change a 'Maybe' value to any 'Alternative'.
maybeToAlt :: Alternative m => Maybe a -> m a
maybeToAlt (Just a) = pure a
maybeToAlt Nothing  = empty

-- | Append lists fairly.
fairAppend :: [a] -> [a] -> [a]
fairAppend (x : xs) (y : ys) = x : y : fairAppend xs ys
fairAppend []       y        = y
fairAppend x        []       = x

-- | Take the only element of a list.
only :: [a] -> a
only [x] = x
only []  = error "GL.Utils.only: []"
only _   = error "GL.Utils.only: too big"

infixl 3 |>

-- | Add a default value for an 'Alternative'.
(|>) :: Alternative f => f a -> a -> f a
f |> a = f <|> pure a
