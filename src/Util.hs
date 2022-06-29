module Util where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Char
import           Data.List
import           System.IO
import           System.Process
import           System.Random
import           System.Directory
import           Control.Monad
import           System.Info


onWindows :: Bool
onWindows = os == "mingw32"

clearScreen :: IO ()
clearScreen = void $ system $ if onWindows then "cls" else "clear"

prompt :: String -> (String -> IO a) -> IO a
prompt message f = do
  putStr message
  hFlush stdout
  getLine >>= f

choice :: Set a -> IO a
choice v = do
  i <- randomRIO (0, Set.size v - 1)
  pure $ Set.elemAt i v

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

copyToClipboard :: String -> IO ()
copyToClipboard text = do
  writeFile "temp" text
  void $ system $ if onWindows
    then "type temp | clip"
    else "cat temp | xclip -selection clipboard"
  removeFile "temp"
