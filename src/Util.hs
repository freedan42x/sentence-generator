module Util where

import           Data.Char                      ( isSpace )
import           Data.List                      ( dropWhileEnd )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           System.Process                 ( system )
import           System.Random                  ( randomRIO )
import           System.Directory               ( removeFile )
import           Control.Monad                  ( void )



clearScreen :: IO ()
clearScreen = void $ system "clear"


prompt :: String -> (String -> IO a) -> IO a
prompt message f = do
  putStr message
  hFlush stdout
  getLine >>= f


getLines :: IO [String]
getLines = do
  getLine >>= \case
    "" -> pure []
    s  -> (s :) <$> getLines


prompts :: String -> ([String] -> IO a) -> IO a
prompts message f = do
  putStr message
  hFlush stdout
  getLines >>= f


confirm :: IO ()
confirm = prompt "[OK] " $ const $ pure ()


choice :: [a] -> IO a
choice xs = do
  let len = length xs
  num <- randomRIO (0, len - 1)
  pure $ xs !! num


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


startsWith :: String -> String -> Bool
startsWith "" _                       = True
startsWith (x : xs) (y : ys) | x == y = startsWith xs ys
startsWith _ _                        = False


newtype Dupe a =
  Dupe { getDupe :: (a, a)
       }


copyToClipboard :: String -> IO ()
copyToClipboard text = do
  writeFile "temp" text
  void $ system "cat temp | xclip -selection clipboard"
  removeFile "temp"
