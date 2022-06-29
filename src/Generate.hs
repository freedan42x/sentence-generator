module Generate where

import           Data.Char                      ( isAlphaNum
                                                , isSpace
                                                , toLower
                                                , isSymbol
                                                )
import           Data.List                      ( find )
import           Data.Functor.Identity          ( Identity(..) )
import           Relude.Extra.Bifunctor         ( bimapBoth )
import           Control.Monad                  ( replicateM )
import           System.Random                  ( randomRIO )
import           Text.Read                      ( readMaybe )
import           Text.Printf                    ( printf )
import           Util                           ( choice
                                                , Dupe(..)
                                                )
import           Collection                     ( Collection
                                                , collection
                                                )



data ChanceT m =
  ChanceT { getChanceT :: m Int
          , getF :: String -> String
          }

type Chance = ChanceT Identity

type ChanceR = ChanceT Dupe


chance :: Int -> (String -> String) -> Chance
chance = ChanceT . Identity


chanceR :: (Int, Int) -> (String -> String) -> ChanceR
chanceR = ChanceT . Dupe


getChance :: Chance -> Int
getChance = runIdentity . getChanceT


getChanceR :: ChanceR -> (Int, Int)
getChanceR = getDupe . getChanceT


fromChances :: [Chance] -> [ChanceR]
fromChances = helper 0
 where
  helper _ []       = []
  helper s (c : cs) = chanceR (s, s + cc) (getF c) : helper (s + cc) cs
    where cc = getChance c


withChances :: [Chance] -> IO (String -> String)
withChances chances = do
  num <- randomRIO @Int (0, 99)
  pure
    $ case
        find (uncurry (\from to -> num >= from && num <= to) . getChanceR)
          $ fromChances chances
      of
        Just cc -> getF cc
        _       -> id


generateCollection :: String -> Collection
generateCollection =
  {- map (map toLower). -}
                     words
  . filter (isAlphaNum <|> isSpace <|> isSymbol <|> ('\'' ==))
 where
  (<|>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
  p <|> q = \x -> p x || q x


rules :: [Chance]
rules =
  [ chance 9 (<> ",")
  , chance 8 (<> ".")
  -- , chance 3  (<> "!")
  , chance 2  (<> "...")
  , chance 3  (<> " —")
  -- , chance 1  (\s -> "«" <> s <> "»")
  , chance 1  (<> ";")
  , chance 1  (<> ":")
  -- , chance 3  (<> "?")
  ]


generateTextF :: (Collection -> Collection) -> Int -> IO String
generateTextF f len = do
  funcs       <- replicateM len $ withChances rules
  resultWords <- replicateM len $ choice $ f collection
  pure $ unwords $ uncurry ($) <$> zip funcs (map yearRule resultWords)
 where
  yearRule s =
    maybe s (\n -> (if n > 1000 then "в " else "") <> show n) $ readMaybe @Int s


generateText :: Int -> IO String
generateText = generateTextF id


generateChinese :: Int -> IO String
generateChinese len = do
  chars <- readFile "chinese"
  replicateM len $ choice chars

generateMenu :: [(String, String)] -> String
generateMenu list =
  foldr
      (\(key, desc) rest -> printf
        "[%s]%s%s\n%s"
        key
        (replicate (maxLenKey - length key + 2) ' ')
        desc
        rest
      )
      ""
      list
    <> replicate (maxLenKey + maxLenDesc + 4) '-'
  where (maxLenKey, maxLenDesc) = bimapBoth (maximum . map length) $ unzip list
