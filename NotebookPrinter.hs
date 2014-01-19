module NotebookPrinter
  (
    printNotebook
  , printNotebookFiltered
  ) where

import NotebookModel
import EventModel

-- Print all entries in notebook

printNotebook :: Notebook -> IO ()
printNotebook = printNotebookIterate 0

printNotebookIterate :: Int -> Notebook -> IO ()
printNotebookIterate _ []     = putStrLn ""
printNotebookIterate 0 xs     = do
                                  printNotebookHeader
                                  printNotebookIterate 1 xs
printNotebookIterate n (x:xs) = do
                                  printEntry n x
                                  printNotebookIterate (n+1) xs

-- Print selected entries in notebook

printNotebookFiltered :: (Entry -> Bool) -> Notebook -> IO ()
printNotebookFiltered = printNotebookFilteredIterate 0

printNotebookFilteredIterate :: Int
                                -> (Entry -> Bool)
                                -> Notebook
                                -> IO ()
printNotebookFilteredIterate _ _ [] = putStrLn ""
printNotebookFilteredIterate 0 f xs
      = do
          printNotebookHeader
          printNotebookFilteredIterate 1 f xs
printNotebookFilteredIterate n f (x:xs)
      | f x       = do
                      printEntry n x
                      printNotebookFilteredIterate (n+1) f xs
      | otherwise =   printNotebookFilteredIterate (n+1) f xs

-- Standard start

printNotebookHeader :: IO ()
printNotebookHeader = putStrLn $
                      "No :: Name :: Date & time"
                      ++ " :: DONE/TODO :: Recurrence"

-- Render entry

printEntry :: Int -> Entry -> IO ()
printEntry n e = putStrLn $
                  show n ++ " :: " ++
                  (name . event) e ++ " :: " ++
                  show ( (dateTime . event) e) ++
                  " :: " ++ printDoneUndone e ++
                  " :: " ++ show ((recurrence . event) e)

-- Render status

printDoneUndone :: Entry -> String
printDoneUndone e | done e    = "DONE"
                  | otherwise = "TODO"
