module Main where

import Control.Monad (when)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.Random (getStdGen, getStdRandom)
import Text.Read (readMaybe)

import Monoids

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file, readMaybe -> Just len] -> do
      src <- readFile file
      when (length (words src) <= 1) $
        putStrLn "error: file should contain at least two words"
      let freqs = train src
      k <- getStdRandom (fromJust . randomElem (Map.keys (fromFreqMap freqs)))
      putStrLn . unwords . walk len k freqs =<< getStdGen
    _ -> putStrLn "usage: talk <file> <max-output-length>"
