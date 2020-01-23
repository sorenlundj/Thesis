{-# LANGUAGE FlexibleContexts  #-}

module Types where

import Data.Text (Text)

data Entity =
  Entity { eType  :: Type
         , eId    :: Int
         , eName  :: String
         , eState :: [String]
         } deriving (Show, Eq)

data Type = Ship
          | Service
          | Company
          deriving (Show, Eq)

data Relation =
  Relation { rFrom :: Entity
           , rTo   :: Entity
           , dep   :: [Dependency]
           } deriving (Show, Eq)

data Dependency =
  Dependency { desc :: String
             , val  :: Bool -- (This should be an anonymous function)
             } deriving (Show, Eq)
