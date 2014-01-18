module Menu
  (
    welcomeScreen
  ) where

import Data.Char
import Data.Maybe
import Data.Time
import System.Exit
import qualified Data.Map as Map

import EventModel
import NotebookModel
import NotebookPrinter
import Today

-- Entry to...

welcomeScreen :: Day -> IO ()
welcomeScreen today = do
  printToday today
  goToTheNextOne Welcome "" today []

printToday :: Day -> IO ()
printToday today = do
  putStrLn ""
  putStrLn "Today we have:"
  print today

-- ...the state machinery!

data Screen = Welcome
            | PrintAll
            | PrintTodays
            | MarkAsDone
            | Delete
            | NewEntry

goToTheNextOne :: Screen -> String -> Day -> Notebook -> IO ()

-- Welcome screen

goToTheNextOne Welcome "a" today notebook = do
  putStrLn ""
  printNotebook notebook
  putStrLn "(R)eturn / (M)ark as done / (D)elete"
  choice <- getLine
  goToTheNextOne PrintAll (map toLower choice) today notebook

goToTheNextOne Welcome "t" today notebook = do
  putStrLn ""
  printNotebookFiltered (isForToday today) notebook
  putStrLn "(R)eturn / (M)ark as done / (D)elete"
  choice <- getLine
  goToTheNextOne PrintTodays (map toLower choice) today notebook

goToTheNextOne Welcome "d" _ notebook = do
  today <- setToday
  printToday today
  goToTheNextOne Welcome "" today notebook

goToTheNextOne Welcome "n" today notebook =
  goToTheNextOne NewEntry "" today notebook

goToTheNextOne Welcome "l" today _ = do
  notebook <- loadFromFile
  putStrLn "successfully loaded notebook.json"
  goToTheNextOne Welcome "" today notebook

goToTheNextOne Welcome "s" today notebook = do
  putStrLn ""
  saveToFile notebook
  putStrLn "successfully saved to notebook.json"
  goToTheNextOne Welcome "" today notebook

goToTheNextOne Welcome "p" today notebook = do
  printNotebookFiltered (isDone True) notebook
  goToTheNextOne Welcome "" today notebook

goToTheNextOne Welcome "q" _ _ = exitSuccess

goToTheNextOne Welcome _   today notebook = do
  putStrLn ""
  putStrLn "(A)ll / (T)oday's / (D)ate / (N)ew entry /"
  putStrLn "(L)oad from file / (S)ave to file / (P)ast /"
  putStrLn "(Q)uit"
  choice <- getLine
  goToTheNextOne Welcome (map toLower choice) today notebook

-- All entries screen

goToTheNextOne PrintAll "r" today notebook =
  goToTheNextOne Welcome "" today notebook

goToTheNextOne PrintAll "m" today notebook =
  goToTheNextOne MarkAsDone "a" today notebook

goToTheNextOne PrintAll "d" today notebook =
  goToTheNextOne Delete "a" today notebook

goToTheNextOne PrintAll _ today notebook =
  goToTheNextOne Welcome "a" today notebook

-- Today's entries screen

goToTheNextOne PrintTodays "r" today notebook =
  goToTheNextOne Welcome "" today notebook

goToTheNextOne PrintTodays "m" today notebook =
  goToTheNextOne MarkAsDone "t" today notebook

goToTheNextOne PrintTodays "d" today notebook =
  goToTheNextOne Delete "t" today notebook

goToTheNextOne PrintTodays _ today notebook =
  goToTheNextOne Welcome "t" today notebook

-- Mark entry as done screen

goToTheNextOne MarkAsDone allOrTodays today notebook = do
  putStrLn ""
  putStrLn "Choose No to mark as done:"
  choice <- getLine
  let number = read choice :: Int
  goToTheNextOne Welcome allOrTodays
                 today (markAsDone number notebook)

-- Delete entry screen

goToTheNextOne Delete allOrTodays today notebook = do
  putStrLn ""
  putStrLn "Choose No to delete for good:"
  choice <- getLine
  let number = read choice :: Int
  goToTheNextOne Welcome allOrTodays
                 today (removeEntry number notebook)

-- New entry starting sceen, powers off defining steps

goToTheNextOne NewEntry _ today notebook = do
  newEntry <- defineEntry NameStep Map.empty
  putStrLn "added new entry"
  goToTheNextOne Welcome "" today (addEntry newEntry notebook)

-- New entry definition steps

data Step = NameStep
          | YearStep
          | MonthStep
          | DayStep
          | HourStep
          | MinuteStep
          | ReocurrenceStep
          | DefinitiveStep

defineEntry :: Step -> Map.Map String String -> IO Entry

defineEntry NameStep params = do
  putStrLn ""
  putStrLn "Name the big event:"
  nameString <- getLine
  defineEntry YearStep $ Map.insert "name" nameString params

defineEntry YearStep params = do
  putStrLn ""
  putStrLn "Gimme the year:"
  year <- getLine
  if inputCorrect YearStep year
    then
      defineEntry MonthStep $ Map.insert "year" year params
    else do
      putStrLn "Stop living in the past, mate!"
      defineEntry YearStep params

defineEntry MonthStep params = do
  putStrLn ""
  putStrLn "Gimme the month (1-12):"
  month <- getLine
  if inputCorrect MonthStep month
    then
      defineEntry DayStep $ Map.insert "month" month params
    else do
      putStrLn "Learn your months, mate!"
      defineEntry MonthStep params

defineEntry DayStep params = do
  putStrLn ""
  putStrLn "Gimme the day:"
  day <- getLine
  if dayCorrect params day
    then
      defineEntry HourStep $ Map.insert "day" day params
    else do
      putStrLn "Learn your days, mate!"
      defineEntry DayStep params

defineEntry HourStep params = do
  putStrLn ""
  putStrLn "What time? Hour (0-23):"
  hours <- getLine
  if inputCorrect HourStep hours
    then
      defineEntry MinuteStep $ Map.insert "hour" hours params
    else do
      putStrLn "Learn your hours, mate!"
      defineEntry HourStep params

defineEntry MinuteStep params = do
  putStrLn ""
  putStrLn "What time? Minutes (0-59):"
  minutes <- getLine
  if inputCorrect MinuteStep minutes
    then
      defineEntry ReocurrenceStep
                  $ Map.insert "minutes" minutes params
    else do
      putStrLn "Learn your minutes, mate!"
      defineEntry MinuteStep params

defineEntry ReocurrenceStep params = do
  putStrLn ""
  putStrLn "Repeat? (D)aily / (W)eekly / (M)onthly / (Y)early /(N)o:"
  reocIn <- getLine
  let reoc = map toLower reocIn
  if reoc `elem` ["d", "w", "m", "y", "n"]
    then
      defineEntry DefinitiveStep $ Map.insert "reoc" reoc params
    else do
      putStrLn "Choose wisely!"
      defineEntry ReocurrenceStep params

defineEntry DefinitiveStep params = do
  let eventName = fromJust $ Map.lookup "name" params :: String
  let year = read $ fromJust $ Map.lookup "year" params :: Integer
  let month = read $ fromJust $ Map.lookup "month" params :: Int
  let day = read $ fromJust $ Map.lookup "day" params :: Int
  let hour = read $ fromJust $ Map.lookup "hour" params :: Integer
  let minutes = read $ fromJust
                     $ Map.lookup "minutes" params :: Integer
  let reoc = decodeRecurrence
             $ fromJust $ Map.lookup "reoc" params :: EventRecurrence
  let entryEvent = Event eventName (eventDateTime (year, month, day)
                         (hour,minutes,0)) reoc
  return $ Entry entryEvent False

decodeRecurrence :: String -> EventRecurrence
decodeRecurrence "n" = None
decodeRecurrence "d" = Daily
decodeRecurrence "w" = Weekly
decodeRecurrence "m" = Monthly
decodeRecurrence "y" = Yearly
decodeRecurrence _   = undefined

-- User input checks

dayCorrect :: Map.Map String String -> String -> Bool
dayCorrect params y
    | not $ all isNumber y = False
    | otherwise            = number > 0 && number <= maxDay
    where number = read y :: Int
          maxDay = maxDayForMonth params

inputCorrect :: Step -> String -> Bool

inputCorrect YearStep y
    | not $ all isNumber y = False
    | otherwise            = number >= 2014
    where number = read y :: Integer

inputCorrect MonthStep m
    | not $ all isNumber m = False
    | otherwise            = number >= 1 && number <= 13
    where number = read m :: Int

inputCorrect HourStep h
    | not $ all isNumber h = False
    | otherwise            = number >= 0 && number <= 23
    where number = read h :: Int

inputCorrect MinuteStep m
    | not $ all isNumber m = False
    | otherwise            = number >= 0 && number <= 59
    where number = read m :: Int

