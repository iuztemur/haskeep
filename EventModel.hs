{-# LANGUAGE OverloadedStrings #-}

module EventModel
  (
    eventDateTime
  , EventRecurrence(..)
  , Event(..)
  , nextOccurence
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
                     deriving (Show, Eq)

data Event = Event { name       :: EventName
                   , dateTime   :: EventDateTime
                   , recurrence :: EventRecurrence
                   } deriving (Show)

-- Recurrence

nextDay :: EventDateTime -> EventDateTime
nextDay edt = UTCTime ( (succ . utctDay) edt )
                      ( utctDayTime edt )

nextWeek :: EventDateTime -> EventDateTime
nextWeek edt = iterate nextDay edt !! 7

nextMonth :: EventDateTime -> EventDateTime
nextMonth edt = UTCTime ( addGregorianMonthsClip 1 (utctDay edt) )
                        ( utctDayTime edt )

nextYear :: EventDateTime -> EventDateTime
nextYear edt = UTCTime ( addGregorianYearsClip 1 (utctDay edt) )
                       ( utctDayTime edt )

nextOccurence :: Event -> Event
nextOccurence e | recurrence e == Daily
                  = Event ( name e )
                          ( (nextDay . dateTime) e )
                          ( recurrence e )
                | recurrence e == Weekly
                  = Event ( name e )
                          ( (nextWeek . dateTime) e )
                          ( recurrence e )
                | recurrence e == Monthly
                  = Event ( name e )
                          ( (nextMonth . dateTime) e )
                          ( recurrence e )
                | recurrence e == Yearly
                  = Event ( name e )
                          ( (nextYear . dateTime) e )
                          ( recurrence e )
                | otherwise
                  = undefined

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
                         ( parseRecurrence <$> v .: "recurrence")
  parseJSON _          = empty
