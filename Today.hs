module Today
  (
    setToday
  , maxDayForMonth
  ) where

import qualified Data.Map as Map
import Data.Time
import Data.Maybe

setToday :: IO Day
setToday = askWhatDayItIs YearStep Map.empty

data Step = YearStep | MonthStep | DayStep | DefinitiveStep

askWhatDayItIs :: Step -> Map.Map String String -> IO Day

askWhatDayItIs YearStep _ = do
  putStrLn ""
  putStrLn "Gimme the year:"
  yearString <- getLine
  let year = read yearString :: Integer
  if year > 2013
    then
      askWhatDayItIs MonthStep
                      $ Map.insert "year" yearString Map.empty
    else do
      putStrLn "Stop living in the past, mate!"
      askWhatDayItIs YearStep Map.empty

askWhatDayItIs MonthStep params = do
  putStrLn ""
  putStrLn "Gimme the month:"
  monthString <- getLine
  let month = read monthString :: Int
  if month > 0 && month < 13
    then
      askWhatDayItIs DayStep
                     $ Map.insert "month" monthString params
    else do
      putStrLn "Learn your months, mate!"
      askWhatDayItIs MonthStep params

askWhatDayItIs DayStep params = do
  putStrLn ""
  putStrLn "Gimme the day:"
  monthString <- getLine
  let day = read monthString :: Int
  let maxDay = maxDayForMonth params
  if day > 0 && day <= maxDay
    then
      askWhatDayItIs DefinitiveStep
                     $ Map.insert "day" monthString params
    else do
      putStrLn $ "The month has " ++ show maxDay ++ " days."
      putStrLn "So learn your days, mate!"
      askWhatDayItIs DayStep params

askWhatDayItIs DefinitiveStep params = do
  let year = read $ fromJust $ Map.lookup "year" params :: Integer
  let month = read $ fromJust $ Map.lookup "month" params :: Int
  let day = read $ fromJust $ Map.lookup "day" params :: Int
  return $ fromGregorian year month day

-- One utility

maxDayForMonth :: Map.Map String String -> Int
maxDayForMonth params = do
  let year = read $ fromJust $ Map.lookup "year" params :: Integer
  let month = read $ fromJust $ Map.lookup "month" params :: Int
  gregorianMonthLength year month