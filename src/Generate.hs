module Generate where

import           Data.List
import           Data.List.Split
import           Data.Function
import           Data.Functor
import           Text.Read                      ( readMaybe )
import           Control.Monad
import           System.Random
import           Vocabulary
import           Util


type Chances = [(Int, String)]
type Ranges = [((Int, Int), String)]

readChances :: IO Chances
readChances = do
  content <- readFile "config"
  pure $ lines content <&> \line -> splitOn "%" line & \case
    [ps, suffix] -> case readMaybe @Int ps of
      Just p -> case readMaybe @String $ trim suffix of
        Just s -> (p, s)
        _      -> error "The string should be enquoted"
      _ -> error "The chance should be an integer"
    _ -> error "Error in config file"

genRanges :: Chances -> Ranges
genRanges = helper 0 where
  helper _   []            = []
  helper acc ((x, s) : ps) = ((acc, acc + x), s) : helper (acc + x) ps

genChanceFunc :: Ranges -> IO (String -> String)
genChanceFunc ranges = do
  c <- randomRIO (0, 99)
  pure $ \s -> case find (\((x, y), _) -> c >= x && c < y) ranges of
    Just (_, suffix) -> s <> suffix
    _                -> s

genText :: Vocab -> Int -> Ranges -> IO String
genText v n ranges = do
  funcs       <- replicateM n $ genChanceFunc ranges
  resultWords <- replicateM n $ choice v
  pure $ unwords $ uncurry ($) <$> zip funcs resultWords
