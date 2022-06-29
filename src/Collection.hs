module Collection where

import           Text.Read                      ( readMaybe )
import           Data.List                      ( nub )
import           System.IO.Unsafe               ( unsafePerformIO )



type Collection = [String]


mergeCollection :: Collection -> Collection -> Collection
mergeCollection xs ys = nub $ xs <> ys


readCollection :: IO Collection
readCollection = do
  collectionS <- readFile "english"
  pure $ case readMaybe @Collection collectionS of
    Just coll -> coll
    _         -> mempty


collection :: Collection
collection = unsafePerformIO readCollection


collectionLen :: IO Int
collectionLen = do
  col <- readCollection
  pure $ length col