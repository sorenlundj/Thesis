{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module SpecParser where

import SpecAst

import Data.Char (isPrint)
import Text.ParserCombinators.Parsec

-- ------------------------------------------------------------------------- --
-- Constants                                                                 --
-- ------------------------------------------------------------------------- --

-- TODO: Erase if not relevant

-- ------------------------------------------------------------------------- --
-- Auxiliary functions                                                       --
-- ------------------------------------------------------------------------- --

-- Reads more than zero Chars
readStr :: String -> Parser String
readStr str = remSpace $ string str

-- Reads more than zero Ints
readInt :: Parser Int
readInt = remSpace $ read <$> many1 digit

-- Reads until the input string is matched
readArbStr :: String -> Parser String 
readArbStr str = remSpace $ manyTill (satisfy isPrint) (lookAhead (readStr str))

-- Non-reads a char
readNoneOf :: String -> Parser Char
readNoneOf ch = remSpace $ noneOf ch

-- Removes spaces
remSpace :: Parser a -> Parser a
remSpace p = do
               a <- p
               spaces
               return a

startTag :: Parser String
startTag = do
             _     <- readStr $ "<"
             sName <- readArbStr $ ">"
             _     <- readStr $ ">"
             return sName                

-- ------------------------------------------------------------------------- --
-- Parse functions                                                           --
-- ------------------------------------------------------------------------- --

-- Top-level parse function
parseSpecList :: Parser SpecList
parseSpecList = do
                  spaces
                  cmds <- parseSubSpec
                  spaces
                  eof
                  return [cmds]

-- Parses arbitrary Specifications
parseSpec :: Parser Spec
parseSpec = do
              sName <- startTag
              sBody <- readArbStr $ "</" -- TODO: SKIPS TO END OF SPEC !!NOT IDEAL!!
-- TODO: Parse SubSpec here
              _     <- readStr $ "</" ++ sName ++ ">"
              return $ Spec sName []

-- Parses arbitrary SubSpecifications
parseSubSpec :: Parser Spec
parseSubSpec = do
                 sName <- startTag
                 sBody <- parseSpecBody
                 _     <- readStr $ "</" ++ sName ++ ">"
                 return $ SubSpec sName sBody

-- ------------------------------------------------------------------------- --
-- Precedence functions                                                      --
-- ------------------------------------------------------------------------- --

precSBody :: Parser Spec
precSBody = undefined

precSubSpec :: Parser Spec
precSubSpec = undefined

precSpec :: Parser Spec
precSpec = undefined

-- ------------------------------------------------------------------------- --
-- Wrapper functions                                                         --
-- ------------------------------------------------------------------------- --

-- Parses a string
parseString :: String -> Either ErrMsg SpecList
parseString input = case parse parseSpecList "" input of
                      Left err  -> Left $ show err
                      Right val -> Right val

-- Parses a file
parseFile :: FilePath -> IO (Either ErrMsg SpecList)
parseFile fp = do s <- readFile fp; return $ parseString s
