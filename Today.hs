module Today
  (
    setToday
  , maxDayForMonth
  ) where

import qualified Data.Map as Map
import Data.Char
import Data.Time
import Data.Maybe

setToday :: IO Day
setToday = askWhatDayItIs YearStep Map.empty

data Step = YearStep | MonthStep | DayStep | DefinitiveStep

askWhatDayItIs :: Step -> Map.Map String String -> IO Day

askWhatDayItIs YearStep params = do
  putStrLn ""
  putStrLn "Gimme the year:"
  year <- getLine
  if inputCorrect YearStep year
    then
      askWhatDayItIs MonthStep $ Map.insert "year" year params
    else do
      putStrLn "Stop living in the past, mate!"
      askWhatDayItIs YearStep Map.empty

askWhatDayItIs MonthStep params = do
  putStrLn ""
  putStrLn "Gimme the month (1-12):"
  month <- getLine
  if inputCorrect MonthStep month
    then
      askWhatDayItIs DayStep $ Map.insert "month" month params
    else do
      putStrLn "Learn your months, mate!"
      askWhatDayItIs MonthStep params

askWhatDayItIs DayStep params = do
  putStrLn ""
  putStrLn "Gimme the day:"
  day <- getLine
  if dayCorrect params day
    then
      askWhatDayItIs DefinitiveStep
                     $ Map.insert "day" day params
    else do
      putStrLn "Learn your days, mate!"
      askWhatDayItIs DayStep params

askWhatDayItIs DefinitiveStep params = do
  let year = read $ fromJust $ Map.lookup "year" params :: Integer
  let month = read $ fromJust $ Map.lookup "month" params :: Int
  let day = read $ fromJust $ Map.lookup "day" params :: Int
  return $ fromGregorian year month day

-- User input checks

dayCorrect :: Map.Map String String -> String -> Bool
dayCorrect params y
    | not $ all isNumber y = False
    | otherwise            = number > 0 && number <= maxDay
    where number = read y :: Int
          maxDay = maxDayForMonth params

maxDayForMonth :: Map.Map String String -> Int
maxDayForMonth params = do
  let year = read $ fromJust $ Map.lookup "year" params :: Integer
  let month = read $ fromJust $ Map.lookup "month" params :: Int
  gregorianMonthLength year month

inputCorrect :: Step -> String -> Bool

inputCorrect YearStep y = all isNumber y

inputCorrect MonthStep m
    | not $ all isNumber m = False
    | otherwise            = number >= 1 && number <= 13
    where number = read m :: Int
