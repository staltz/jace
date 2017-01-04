module Main where

import Lib (compile)
import System.Environment (getArgs)
import Data.Text (toUpper, pack, unpack)
import Debug.Trace (trace)

(|>) = flip ($)

main :: IO ()
main = do
  args <- getArgs
  if length args > 0 then do
    let inputFileName = head args
    inputFileContents <- readFile inputFileName
    inputFileContents |> compile |> putStrLn
  else
    putStrLn "Please provide an input file as an argument to this program"
