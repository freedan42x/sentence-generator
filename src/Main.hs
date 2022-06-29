module Main where

import           GHC.IO.Encoding                ( setLocaleEncoding
                                                , utf8
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           System.Directory               ( copyFile
                                                , removeFile
                                                )

import           System.Exit                    ( exitSuccess )

import           System.IO                      ( stdin
                                                , hSetEncoding
                                                , latin1
                                                )
import           System.Process                 ( system )
import           Text.Read                      ( readMaybe )
import           Text.Printf                    ( printf )
import           Util                           ( prompt
                                                , prompts
                                                , confirm
                                                , clearScreen
                                                , trim
                                                , copyToClipboard
                                                , startsWith
                                                , endsWith
                                                )
import           Generate                       ( generateCollection
                                                , generateText
                                                , generateChinese
                                                , generateMenu
                                                )
import           Collection                     ( Collection
                                                , readCollection
                                                , mergeCollection
                                                , collectionLen
                                                )




collectionActionIO :: (Collection -> Collection -> Collection) -> IO ()
collectionActionIO f = do
  collection <- readCollection
  prompt "> " $ \text ->
    writeFile "result" $ show $ f collection (generateCollection text)
  copyFile "result" "english"
  removeFile "result"


addWordsIO :: IO ()
addWordsIO = collectionActionIO mergeCollection


addWordsMultiLineIO :: IO ()
addWordsMultiLineIO = do
  collection <- readCollection
  prompts "> " $ \text -> writeFile "result" $ show $ mergeCollection
    collection
    (generateCollection $ unlines text)
  copyFile "result" "english"
  removeFile "result"


removeWordsIO :: IO ()
removeWordsIO = collectionActionIO
  $ \curCol removedWords -> filter (`notElem` removedWords) curCol


generateTextIO :: Int -> IO ()
generateTextIO len = do
  text <- generateText len
  putStrLn text
  copyToClipboard text
  putStrLn $ replicate 26 '-'
  prompt "[r]         generate again\n\
         \> " $ \s -> case trim s of
    "r" -> clearScreen >> generateTextIO len
    _   -> pure ()


generateChineseIO :: Int -> IO ()
generateChineseIO len = do
  text <- generateChinese len
  putStrLn text
  copyToClipboard text
  putStrLn $ replicate 26 '-'
  prompt "[r]         generate again\n\
         \> " $ \s -> case trim s of
    "r" -> clearScreen >> generateChineseIO len
    _   -> pure ()

printCollectionIOF :: (Collection -> Collection) -> IO ()
printCollectionIOF f = do
  collection <- f <$> readCollection
  let text = unwords collection
  copyToClipboard text
  putStrLn text
  confirm


printCollectionIO :: IO ()
printCollectionIO = printCollectionIOF id


menu :: String
menu = generateMenu
  [ ("a"           , "add words to collection")
  , ("A"           , "add words with multiline text support")
  , ("d"           , "remove words from collection")
  , ("g <len>"     , "generate text")
  , ("c <len>"     , "generate chinese text")
  , ("p"           , "print collection")
  , ("ps <pattern>", "print filtered collection (startsWith)")
  , ("pe <pattern>", "print filtered collection (endsWith)")
  , ("q"           , "exit program")
  ]


askActionIO :: IO ()
askActionIO = do
  clearScreen
  colLen <- collectionLen
  prompt (menu <> printf "\n(english:%d) $ " colLen) $ \s ->
    clearScreen >> case words s of
      ["a"]          -> addWordsIO
      ["A"]          -> addWordsMultiLineIO
      ["d"]          -> removeWordsIO
      ["g", len]     -> maybe askActionIO generateTextIO $ readMaybe @Int len
      ["c", len]     -> maybe askActionIO generateChineseIO $ readMaybe @Int len
      ["p"]          -> printCollectionIO
      ["ps", pat]    -> printCollectionIOF $ filter $ startsWith pat
      ["pe", pat]    -> printCollectionIOF $ filter $ endsWith pat
      ["q"]          -> exitSuccess
      _              -> pure ()


main :: IO ()
main = do
  void $ system "chcp 65001"
  -- setLocaleEncoding utf8
  -- hSetEncoding stdin latin1
  forever askActionIO