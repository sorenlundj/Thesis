{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)

data Entity =
  Entity { entityType  :: Type
         , entityId    :: Int
         , entityName  :: Text
         , entityState :: [Text]
         } deriving (Show, Eq)

data Type = Ship
          | Service
          | Company
          deriving (Show, Eq)

data Relation =
  Relation { relationFrom :: Entity
           , relationTo   :: Entity
           , dependency   :: [Dependency]
           } deriving (Show, Eq)

data Dependency =
  Dependency { desc :: Text
             , dep  :: Bool -- (This should be an anonymous function)
             } deriving (Show, Eq)
