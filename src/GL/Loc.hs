{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module GL.Loc
  ( module GL.Loc
  )
where

import           Control.Lens
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as P
import           Text.Megaparsec                ( SourcePos(..) )

import           GL.Utils

data Loc = Loc
    { _locPos :: P.SourcePos
    , _locDuring :: Text
    , _locAfter :: Text
    }
  deriving (Eq, Ord)

makeLenses ''Loc

instance Show Loc where
  show = T.unpack . locPretty

emptyLoc :: FilePath -> Loc
emptyLoc fp =
  Loc { _locPos = P.initialPos fp, _locDuring = "", _locAfter = "" }

infixl 5 <++

(<++) :: SourcePos -> Text -> SourcePos
(<++) = T.foldl $ \p -> \case
  '\t' -> p { sourceColumn = sourceColumn p <> P.defaultTabWidth }
  '\n' -> p { sourceLine = sourceLine p <> P.pos1, sourceColumn = P.pos1 }
  _    -> p { sourceColumn = sourceColumn p <> P.pos1 }

locPretty :: Loc -> Text
locPretty Loc {..} =
  T.pack (P.sourcePosPretty _locPos)
    <> " spelled "
    <> showT _locDuring
    <> " with "
    <> showT _locAfter

recreateToken :: Loc -> Text
recreateToken Loc {..} = _locDuring <> _locAfter

recreateToken' :: Int -> Loc -> Text
recreateToken' tw = replaceTabs tw . recreateToken

addDuring :: Text -> Loc -> Loc
addDuring = (locDuring <>~)

addAfter :: Text -> Loc -> Loc
addAfter = (locAfter <>~)

commit :: Loc -> Loc
commit l@Loc {..} = l { _locPos    = _locPos <++ _locDuring <> _locAfter
                      , _locDuring = ""
                      , _locAfter  = ""
                      }

data LocT t = LocT
    { _tokVal :: t
    , _tokLoc :: Loc
    }
  deriving (Eq, Ord)

makeLenses ''LocT
