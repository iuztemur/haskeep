{-# LANGUAGE OverloadedStrings #-}

module NotebookModel
  (
    Entry(..)
  , Notebook
  ) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson

import EventModel

data Entry = Entry { event :: Event
                   , done  :: Bool
                   } deriving (Show)

instance ToJSON Entry where
  toJSON (Entry e d) = object [ "event" .= e
                              , "done"  .= d ]

instance FromJSON Entry where
  parseJSON (Object v) = Entry <$>
                         v .: "event" <*>
                         v .: "done"
  parseJSON _          = empty

type Notebook = [Entry]