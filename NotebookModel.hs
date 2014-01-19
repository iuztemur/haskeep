{-# LANGUAGE OverloadedStrings #-}

module NotebookModel
  (
    Entry(..)
  , Notebook
  , addEntry
  , removeEntry
  , markAsDone
  , saveToFile
  , loadFromFile
  , isForToday
  , isDone
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Time
import Data.Maybe

import EventModel

-- Entry in the notebook

type Done = Bool

data Entry = Entry { event :: Event
                   , done  :: Done
                   } deriving (Show)

-- Notebook full of entries

type Notebook = [Entry]

addEntry :: Entry -> Notebook -> Notebook
addEntry e n = e : n

removeEntry :: Int -> Notebook -> Notebook
removeEntry _ []     = []
removeEntry 1 (_:xs) = xs
removeEntry n (x:xs) = x : removeEntry (n-1) xs

markAsDone :: Int -> Notebook -> Notebook
markAsDone _ []     = []
markAsDone 1 (x:xs) | (recurrence . event) x == None
                      = Entry (event x)
                              True
                      : xs
                    | otherwise
                      = Entry ((nextOccurence . event) x)
                              False
                        : xs
markAsDone n (x:xs) = x : markAsDone (n-1) xs

-- Persistance

instance ToJSON Entry where
  toJSON (Entry e d) = object [ "event" .= e
                              , "done"  .= d ]

instance FromJSON Entry where
  parseJSON (Object v) = Entry <$>
                         v .: "event" <*>
                         v .: "done"
  parseJSON _          = empty

saveToFile :: Notebook -> IO ()
saveToFile n = BL.writeFile "notebook.json" $ encode n

loadFromFile :: IO Notebook
loadFromFile = do
  notebookFromFile <- BL.readFile "notebook.json"
  return $ fromJust $ decode notebookFromFile

-- Filters

isForToday :: Day -> Entry -> Bool
isForToday d e = d >= (utctDay . dateTime . event) e
              && (not . done) e

isDone :: Done -> Entry -> Bool
isDone d e = done e == d
