module Main
       (main)
where

import SpecParser
import System.Environment(getArgs)

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
              p <- parseFile file
              putStrLn $ show p
            _ ->
              error "Invalid arguments!"
