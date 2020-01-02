module SpecAst where

data Tag = Tag [(String, String)] String (Maybe [Tag])
         | TagString String
  deriving (Show,Eq)

type ErrMsg = String