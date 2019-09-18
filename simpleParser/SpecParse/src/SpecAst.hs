module SpecAst where

type SpecList = [Spec]

data Spec = Spec SName [Spec]
          | SubSpec SName SBody
    deriving (Eq, Show, Read)

type SName = String
type SBody = String -- TODO: Divide into lists, Strings, Ints, so forth


type ErrMsg = String
