module SpecAst where

type SpecList = [Spec]

data Spec = Spec SName [Spec]
          | SubSpec SName SBody
          | SBody
    deriving (Eq, Show, Read)

type SName = String
type SBody = String -- TODO: Divide into lists, Strings, Ints, so forth
--           | Int
--           | Char
--           | [String]
--           | [Int]
--           | [Char]



type ErrMsg = String
