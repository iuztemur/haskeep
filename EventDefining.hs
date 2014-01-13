import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import EventModel

decodeRecurrence :: Char -> EventRecurrence
decodeRecurrence 'N' = None
decodeRecurrence 'D' = Daily
decodeRecurrence 'W' = Weekly
decodeRecurrence 'M' = Monthly
decodeRecurrence 'Y' = Yearly
decodeRecurrence _   = undefined

main :: IO ()
main = do

  putStrLn "Event name:"
  eventName <- getLine

  putStrLn "Event year:"
  yearString <- getLine
  let year = read yearString :: Integer

  putStrLn "Event month:"
  monthString <- getLine
  let month = read monthString :: Int

  putStrLn "Event day:"
  dayString <- getLine
  let day = read dayString :: Int

  putStrLn "Event hour:"
  hourString <- getLine
  let hour = read hourString :: Integer

  putStrLn "Event minutes:"
  minutesString <- getLine
  let minutes = read minutesString :: Integer

  putStrLn "Event recurrence (N)one, (D)aily,"
  putStrLn "(W)eekly, (M)onthly, (Y)early:"
  recurrenceString <- getChar
  let eventRecurrence = decodeRecurrence recurrenceString
                                         :: EventRecurrence

  let event = Event eventName (eventDateTime (year,month,day)
                              (hour,minutes,0)) eventRecurrence
  BL.writeFile "kuba-def.json" $ encode event
  putStrLn "-- wrote to file kuba-def.json"
