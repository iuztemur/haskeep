{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Data.Maybe (fromJust)

import EventModel
import NotebookModel

main :: IO ()
main = do

	-- Events
	let firstEvent = Event "w" (eventDateTime (2000,3,3)
							   (22,3,0)) Monthly
	let secondEvent = Event "y" (eventDateTime (2001,1,1)
								(23,7,0)) Monthly

	-- Entry
	let firstEntry = Entry firstEvent False
	BL.writeFile "kuba-entry.json" $ encode firstEntry
	let secondEntry = Entry secondEvent False
	BL.writeFile "kuba-entry.json" $ encode secondEntry
	entryFromFile <- BL.readFile "kuba-entry.json"
	putStrLn ""
	putStrLn "-- entryFromFile:"
	BL.putStrLn entryFromFile
	let decodedEntryFromFile = decode entryFromFile :: Maybe Entry
	putStrLn ""
	putStrLn "-- decodedEntryFromFile:"
	print $ fromJust decodedEntryFromFile

	-- Notebook
	let notebook = [firstEntry, secondEntry] :: Notebook
	BL.writeFile "kuba-notebook.json" $ encode notebook
	notebookFromFile <- BL.readFile "kuba-notebook.json"
	putStrLn ""
	putStrLn "-- notebookFromFile:"
	BL.putStrLn notebookFromFile
	let decodedNotebookFromFile = decode notebookFromFile :: Maybe Notebook
	putStrLn ""
	putStrLn "-- decodedNotebookFromFile:"
	print $ fromJust decodedNotebookFromFile

	putStrLn ""
	putStrLn "-- ok"