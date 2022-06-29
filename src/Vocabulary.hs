module Vocabulary where

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set


type Vocab = Set String

readVocab :: IO (Vocab, Int)
readVocab = do
  content <- readFile "english"
  let v = Set.fromList $ words content
  pure (v, Set.size v)
