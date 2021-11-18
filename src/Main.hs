module Main where

import           Control.Monad                  ( forever
                                                , when
                                                )
import           System.Directory               ( copyFile
                                                , removeFile
                                                )

import           System.Exit                    ( exitSuccess )

import           System.IO                      ( stdin
                                                , hSetEncoding
                                                , latin1
                                                )
import           Text.Read                      ( readMaybe )
import           Text.Printf                    ( printf )
import           Util                           ( prompt
                                                , prompts
                                                , confirm
                                                , clearScreen
                                                , trim
                                                , copyToClipboard
                                                , startsWith
                                                )
import           Generate                       ( generateCollection
                                                , generateText
                                                , generateMenu
                                                )
import           Collection                     ( Collection
                                                , readCollection
                                                , mergeCollection
                                                , collectionName
                                                , collectionPath
                                                , collectionLen
                                                , listCollections
                                                )




collectionActionIO :: (Collection -> Collection -> Collection) -> IO ()
collectionActionIO f = do
  collection <- readCollection
  prompt "> " $ \text ->
    writeFile "result" $ show $ f collection (generateCollection text)
  path <- collectionPath
  copyFile "result" path
  removeFile "result"


addWordsIO :: IO ()
addWordsIO = collectionActionIO mergeCollection


addWordsMultiLineIO :: IO ()
addWordsMultiLineIO = do
  collection <- readCollection
  prompts "> " $ \text -> writeFile "result" $ show $ mergeCollection
    collection
    (generateCollection $ unlines text)
  path <- collectionPath
  copyFile "result" path
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


listCollectionsIO :: IO ()
listCollectionsIO = do
  collections <- listCollections
  putStrLn $ printf
    "Available collections\n\
    \---------------------\n\
    \%s"
    (unlines collections)
  confirm


createCollectionIO :: String -> IO ()
createCollectionIO colName = writeFile ("collections/" <> colName) ""


changeCollectionIO :: String -> IO ()
changeCollectionIO colName = do
  collections <- listCollections
  when (colName `elem` collections) $ writeFile "current_collection" colName


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
  , ("l"           , "list all collections")
  , ("m <name>"    , "create new collection")
  , ("c <name>"    , "change current collection")
  , ("p"           , "print collection")
  , ("ps <pattern>", "print filtered collection (startsWith)")
  , ("q"           , "exit program")
  ]


askActionIO :: IO ()
askActionIO = do
  clearScreen
  name   <- collectionName
  colLen <- collectionLen
  prompt (menu <> printf "\n(%s:%d) $ " name colLen) $ \s ->
    clearScreen >> case words s of
      ["a"]          -> addWordsIO
      ["A"]          -> addWordsMultiLineIO
      ["d"]          -> removeWordsIO
      ["g", len]     -> maybe askActionIO generateTextIO $ readMaybe @Int len
      ["l"]          -> listCollectionsIO
      ["m", colName] -> createCollectionIO colName
      ["c", colName] -> changeCollectionIO colName
      ["p"]          -> printCollectionIO
      ["ps", pat]    -> printCollectionIOF $ filter $ startsWith pat
      ["q"]          -> exitSuccess
      _              -> pure ()


main :: IO ()
main = do
  hSetEncoding stdin latin1
  forever askActionIO
