module Main where

import           Control.Monad
import           System.Process
import           Text.Read
import           Util
import           Vocabulary
import           Generate


main :: IO ()
main = do
  when onWindows $ void $ system "chcp 65001"

  (eng, len) <- readVocab
  ranges     <- genRanges <$> readChances

  clearScreen
  forever $ do
    prompt ("(english:" <> show len <> ") $ ") $ \s ->
      case readMaybe @Int $ trim s of
        Just n -> do
          clearScreen

          text <- genText eng n ranges
          putStrLn text
          copyToClipboard text

          putStr "\n"
        _ ->
          putStrLn "Input error: expected a number of words to be generated\n"

