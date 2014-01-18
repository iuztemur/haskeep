import NotebookModel
import NotebookPrinter
import EventModel
import Data.Time

main :: IO ()
main = do

  -- Events, entries, notebook
  let firstDate   = eventDateTime (2000,3,3) (22,3,0) :: UTCTime
  let firstEvent  = Event "w" firstDate Monthly
  let secondEvent = Event "y" (eventDateTime (2001,1,1)
                    (23,7,0)) Monthly
  let firstEntry  = Entry firstEvent False
  let secondEntry = Entry secondEvent True
  let notebook = [firstEntry, secondEntry] :: Notebook

  -- Print!
  printNotebook notebook
  printNotebookFiltered ((isForToday . utctDay) firstDate) notebook
  printNotebookFiltered (isDone True) notebook

  putStrLn "-- ok"
