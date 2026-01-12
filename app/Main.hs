{- cryptoquote
 
   A cryptography game
-}

module Main where

import Control.Monad (unless)
import Data.Char ( isLower, isUpper)
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Ix ( inRange )
import System.Environment ( getArgs )
import System.Exit (die)
import System.IO
    ( hFlush, hSetBuffering, stdin, stdout, BufferMode(LineBuffering) ) 
import System.Random ( randomRIO ) 
import Text.Read (readMaybe)
import Paths_cryptoquote ( getDataFileName )

import Assoc
    ( Assoc, delete, empty, findWithDefault, insert
    , isKeyOf, isValueOf, modifyWithDefault )

------------------------------------------------------------
-- A CipherDictionary is a partial map from the uppercase English
-- alphabet to the lowercase English alphabet 

type CipherDictionary = Assoc Char Char

-- Decipher a String using a CipherDictionary. This uses decipherChar,
-- and so missing letters are mapped to underscores.

decipher :: CipherDictionary -> String -> String
decipher dict = map (decipherChar dict)

-- Decipher an individual Char via a CipherDictionary. An uppercase
-- letter in the domain of the CipherDictionary is mapped to the
-- corresponding value; an uppercase letter not in the domain of the
-- CipherDictionary is mapped to an underscore; characters that are not
-- uppercase letters (e.g., spaces, punctuation) are mapped to themselves.

decipherChar :: CipherDictionary -> Char -> Char
decipherChar dict c 
  | isUpper c = findWithDefault '_' c dict
  | otherwise = c

------------------------------------------------------------
-- A FrequenceMap maintains character counts by letter.

type FrequencyMap = Assoc Char Int

-- Increment the count associated with a letter in a FrequenceMap,
-- starting at one.
increment :: Char -> FrequencyMap -> FrequencyMap
increment = modifyWithDefault (+1) 1

-- Compute the FrequencyMap associated with a string.
makeFrequencies :: String -> FrequencyMap
makeFrequencies = foldr increment empty . filter isUpper

-- Present a FrequencyMap, ordered from most to least frequent character.
showFrequencies :: FrequencyMap -> String
showFrequencies frequencyMap = 
  let ps = sortBy (comparing (\(key,value) -> (-value, key))) frequencyMap
  in unlines . map (\(key,value) -> [key] ++ ": " ++ show value) $ ps

------------------------------------------------------------
-- Model

data AppRecord = AppRecord 
  { cipherText :: String
  , cipherDictionary :: CipherDictionary
  , dictionary :: [String]
  }

------------------------------------------------------------
-- View

view :: AppRecord -> String
view AppRecord { cipherText = ct, cipherDictionary = cm } =
  let clearText = decipher cm ct 
      cipherLines = lines ct
      clearLines = lines clearText
      fmt cipherLn clearLn = unlines [cipherLn, clearLn, "", ""]
  in
    concat $ zipWith fmt cipherLines clearLines

------------------------------------------------------------
-- REPL

repl :: AppRecord -> IO ()
repl appRecord = do
  putStrLn (view appRecord)
  cmd <- prompt "> "
  case words cmd of
    [] -> repl appRecord
    [[a]]
      | isUpper a ->
          let appRecord' = appRecord { cipherDictionary = delete a (cipherDictionary appRecord)}
          in repl appRecord'
      | True -> do
        putStrLn $ "unrecognized command: " ++ cmd
        repl appRecord
    [[a,b]]
      | isUpper a && isLower b ->
          let appRecord' = appRecord { cipherDictionary = insert a b (cipherDictionary appRecord)}
          in repl appRecord'
    [":letters"] -> do
      putStr $ showFrequencies (makeFrequencies (cipherText appRecord))
      putStrLn ""
      repl appRecord
    [":match",target] -> do
      let matches = findWords (cipherDictionary appRecord) target (dictionary appRecord) 
          these = take 10 matches
          those = drop 10 matches
      putStrLn . unlines $ these
      unless (null those) $ do
        putStrLn $ "and " ++ show (length those) ++ " others."
      repl appRecord
    [":show"] -> do
      putStrLn (decipher (cipherDictionary appRecord) (cipherText appRecord))
      putStrLn ""
      repl appRecord
    [":quit"] -> pure ()
    _ -> do
      putStrLn $ "unrecognized command: " ++ cmd
      repl appRecord

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

findWords :: CipherDictionary -> String -> [String]  -> [String]
findWords dict target = filter (go dict target) where
  go _ [] [] = True
  go d (a:as) (b:bs)
    | a `isKeyOf` d = findWithDefault '_' a d == b && go d as bs
    | b `isValueOf` d = False
    | otherwise = go (insert a b d) as bs 
  go _ _ _ = False

------------------------------------------------------------
-- main

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  quotationFilePath <- getDataFileName "quotations.enc"
  qs <- readFile quotationFilePath
  let quotes :: [String]
      quotes = map read . lines $ qs
      numQuotes = length quotes
  dictFilePath <- getDataFileName "words.txt"
  dict <- lines <$> readFile dictFilePath

  args <- getArgs
  case args of
    [] -> do
      ix <- randomRIO (0,numQuotes-1)
      let appRecord = AppRecord { cipherText = quotes !! ix 
                                , cipherDictionary = empty 
                                , dictionary = dict }
      repl appRecord
    [str] -> case readMaybe str :: Maybe Int of
      Nothing -> usage
      Just n
        | inRange (0,numQuotes-1) n-> do
            let appRecord = AppRecord 
                  { cipherText = quotes !! n
                  , cipherDictionary = empty 
                  , dictionary = dict }
            repl appRecord
        | otherwise -> die "cryptoquote: quote index out of range"
    _ -> usage

usage :: IO ()
usage = do
  die "usage: cryptoquote [n]"
