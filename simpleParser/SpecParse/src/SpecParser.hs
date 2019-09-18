{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module SpecParser where

import SpecAst

import Data.Char (isPrint)
import Text.ParserCombinators.Parsec


-- ------------------------------------------------------------------------- --
-- Auxiliary functions                                                       --
-- ------------------------------------------------------------------------- --

-- Reads more than zero Chars
readString :: String -> Parser String
readString str = remSpace $ string str

-- Reads more than zero Ints
readInt :: Parser Int
readInt = remSpace $ read <$> many1 digit

-- Reads the remainder of the current line, along with newline
readArbitraryStringNewline :: Parser String
readArbitraryStringNewline = remSpace $ manyTill (satisfy isPrint) (lookAhead (readString "\n"))

-- Reads the remainder of the current word, along with a space
readArbitraryStringSpace :: Parser String
readArbitraryStringSpace = remSpace $ manyTill (satisfy isPrint) (lookAhead (readString " "))

-- Non-reads a char
readNoneOf :: String -> Parser Char
readNoneOf ch = remSpace $ noneOf ch

-- Removes spaces
remSpace :: Parser a -> Parser a
remSpace p = do
               a <- p
               spaces
               return a

-- ------------------------------------------------------------------------- --
-- Parse functions                                                           --
-- ------------------------------------------------------------------------- --
-- Wrapper function
parseAll :: Parser SpecList
parseAll = do
             spaces
             cmds <- parseSpecs
             spaces
             eof
             return cmds



-- ------------------------------------------------------------------------- --
-- Wrapper functions                                                         --
-- ------------------------------------------------------------------------- --

-- Parses a string
parseString :: String -> Either ErrMsg SpecList
parseString input = case parse parseAll "" input of
                      Left err  -> Left $ show err
                      Right val -> Right val

-- Parses a file
parseFile :: FilePath -> IO (Either ErrMsg SpecList)
parseFile fp = do s <- readFile fp; return $ parseString s
