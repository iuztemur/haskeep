{-# LANGUAGE OverloadedStrings #-}

module EventModel
  (
    eventDateTime
  , EventRecurrence(..)
  , Event(..)
  ) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Time
import Data.Aeson

-- Data model

type Year       = Integer
type Month      = Int
type DayOfMonth = Int
type Date       = (Year, Month, DayOfMonth)

type Hours      = Integer
type Minutes    = Integer
type Seconds    = Integer
type Hour       = (Hours, Minutes, Seconds)

timeToDiffTime :: Hours -> Minutes -> Seconds -> DiffTime
timeToDiffTime h m s = secondsToDiffTime
                     $ h * 3600
                     + m * 60
                     + s

eventDateTime :: Date -> Hour -> UTCTime
eventDateTime (y, mo, d) (h, mi, s) = UTCTime
                                      (fromGregorian y mo d)
                                      (timeToDiffTime h mi s)

type EventName       = String
type EventDateTime   = UTCTime
data EventRecurrence = None
                     | Daily
                     | Weekly
                     | Monthly
                     | Yearly
                     deriving (Show)

data Event = Event { name       :: EventName
                   , dateTime   :: EventDateTime
                   , recurrence :: EventRecurrence
                   } deriving (Show)

-- Data persistence

renderRecurrence :: EventRecurrence -> String
renderRecurrence None    = "None"
renderRecurrence Daily   = "Daily"
renderRecurrence Weekly  = "Weekly"
renderRecurrence Monthly = "Monthly"
renderRecurrence Yearly  = "Yearly"

parseRecurrence :: String -> EventRecurrence
parseRecurrence "None"    = None
parseRecurrence "Daily"   = Daily
parseRecurrence "Weekly"  = Weekly
parseRecurrence "Monthly" = Monthly
parseRecurrence "Yearly"  = Yearly

instance ToJSON Event where
  toJSON (Event n d r) = object [ "name"       .= n
                                , "dateTime"   .= d
                                , "recurrence" .= renderRecurrence r ]

instance FromJSON Event where
  parseJSON (Object v) = Event <$>
                         v .: "name" <*>
                         v .: "dateTime" <*>
                         (parseRecurrence <$> v .: "recurrence")
  parseJSON _          = empty
