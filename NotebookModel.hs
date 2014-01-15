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
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Maybe (fromJust)

import EventModel

-- Entry in the notebook

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
-- markAsDone 1 (x:xs) = nextOccurence x : xs
markAsDone 1 (x:xs) | (recurrence . event) x == None
                      = xs
                    | otherwise
                      = Entry ((nextOccurence . event) x)
                              False
                        : xs
markAsDone n (x:xs) = x : markAsDone (n-1) xs

-- Persistance

saveToFile :: Notebook -> IO ()
saveToFile n = BL.writeFile "notebook.json" $ encode n

loadFromFile :: IO Notebook
loadFromFile = do
  notebookFromFile <- BL.readFile "notebook.json"
  return $ fromJust $ decode notebookFromFile