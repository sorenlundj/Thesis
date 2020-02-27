-- | This module defines a simple command line interface for the SubScript
-- interpreter.  If your solution is correct, this module should just
-- work.
module Main
       (main)
where

import MMod
import Checkout

import Data.List(intercalate)
import System.Environment(getArgs)

term :: FSM State Event -> String
term _ = undefined



--data State = NoItems
--           | HasItems (NonEmpty CartItem)
--           | NoCard (NonEmpty CartItem)
--           | CardSelected (NonEmpty CartItem)
--                          Card
--           | CardConfirmed (NonEmpty CartItem)
--                           Card
--           | OrderPlaced
--           deriving (Show, Eq)

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
              pp <- readFile file
              case runFsm (read pp) of
                Left e -> error $ e
                Right res -> putStrLn $ term runFsm (read pp)
            _ ->
              error "Invalid arguments!"
