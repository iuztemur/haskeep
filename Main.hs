import Data.Time
import NotebookModel
import Menu

main :: IO ()
main = do

  currentTime <- getCurrentTime :: IO UTCTime
  let today = utctDay currentTime :: Day

  welcomeScreen today
