module Menu
  (
    welcomeScreen
  ) where

import Data.Char
import Data.Time
import System.Exit
import qualified Data.Map as Map

import NotebookModel
import NotebookPrinter
import Today
import EventDefine

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
