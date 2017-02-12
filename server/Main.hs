module Main where

import SampleAPI (startApp)
import SampleHALAPI (startHyperApp)
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [] -> startApp
    ["--hyper"] -> startHyperApp
    _ -> do putStrLn $ "Usage: " ++ name ++ " [--hyper]"
            putStrLn ""
            putStrLn "--hyper      Runs as HAL server"
