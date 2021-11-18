module Collection where

import           Text.Read                      ( readMaybe )
import           Data.List                      ( nub )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Shelly                         ( shelly
                                                , ls
                                                )



type Collection = [String]


collectionName :: IO String
collectionName = readFile "current_collection"


collectionPath :: IO String
collectionPath = ("collections/" <>) <$> collectionName


mergeCollection :: Collection -> Collection -> Collection
mergeCollection xs ys = nub $ xs <> ys


readCollection :: IO Collection
readCollection = do
  path        <- collectionPath
  collectionS <- readFile path
  pure $ case readMaybe @Collection collectionS of
    Just coll -> coll
    _         -> mempty


collection :: Collection
collection = unsafePerformIO readCollection


collectionLen :: IO Int
collectionLen = do
  col <- readCollection
  pure $ length col


listCollections :: IO [FilePath]
listCollections = do
  collections <- shelly $ ls "collections"
  pure $ (drop $ length "collections/") <$> collections
