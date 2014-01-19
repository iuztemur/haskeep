import Data.Time
import Menu

main :: IO ()
main = do

  currentTime <- getCurrentTime :: IO UTCTime
  let today = utctDay currentTime :: Day

  -- let today = fromGregorian 2000 1 1

  welcomeScreen today
