{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson

import EventModel

main :: IO ()
main = do

  -- Event
  BL.putStrLn $ encode $ Event "w" (eventDateTime (2000,3,3) (22,3,3)) Monthly
  let event = decode "{\"name\":\"w\",\"recurrence\":{\"recurrence\":\"Month\
                    \ly\"},\"dateTime\":\"2000-03-03T22:03:03.\
                    \000Z\"}" :: Maybe Event
  print event

  -- Files: writing
  let encoded = encode $ Event "t" (eventDateTime (2001,5,4) (20,0,0)) Yearly
  let path = "kuba.json" :: FilePath
  BL.writeFile path encoded
  putStrLn "-- encoded and wrote to file:"
  fromFile <- BL.readFile path
  BL.putStrLn fromFile

  -- Files: reading
  let decodedFromFile = decode fromFile :: Maybe Event
  putStrLn "-- read from file and decoded:"
  print decodedFromFile
