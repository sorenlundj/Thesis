module MMod
       (
        runFsm,
        fullLog,
        checkout,
        State(..),
        Event(..),
        FSM
       )

where

import Control.Monad      (foldM)
import Data.List.NonEmpty
import Text.Printf        (printf)

import Types

data State = NoEntsRels
           | HasEnts       (NonEmpty Entity)
           | HasEntsRels   (NonEmpty Entity) (NonEmpty Relation)
           | OrderPlaced
           deriving (Show, Eq)

data Event = SelectE Entity
           | SelectR Relation
           | Confirm
           | PlaceOrder
           | Cancel
           deriving (Show, Eq)

type FSM s e =
  s -> e -> IO s

checkout :: FSM State Event
checkout NoEntsRels (SelectE ent)               = return (HasEnts (ent :| []))
checkout (HasEnts ents) (SelectE ent)           = return (HasEnts (ent <| ents))
checkout (HasEnts ents) (SelectR rel)           = return (HasEntsRels (ents) (rel :| []))
checkout (HasEntsRels ents rels) (SelectR rel)  = return (HasEntsRels (ents) (rel <| rels))
checkout state Cancel                           = return state
checkout state _                                = return state

runFsm :: Foldable f => FSM s e -> s -> f e -> IO s
runFsm = foldM

fullLog :: (Show s, Show e) => FSM s e -> FSM s e
fullLog fsm s e = do
  s' <- fsm s e
  printf "- %s × %s → %s\n" (show s) (show e) (show s')
  return s'
