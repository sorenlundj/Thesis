{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module SpecParser(
  parseFile,
  parseXML,
  Tag(..)
) where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Either
import Data.List
import Data.Char
import SpecAst

-- FAULTS:
-- needs to handle comments
-- XML-descriptor at top of file

escapeCodes = 
  [("gt",'>')
  ,("lt",'<')
  ,("amp",'&')
  ,("quot",'\"')
  ,("apos",'\'')]

spaceOut p = between (many space) (many space) p

attr :: Parser (String, String)
attr = do
  attr <- spaceOut $ many1 (satisfy (liftM2 ((not.).(||)) isSpace (`elem` "=<>")))
  string "="
  value <- spaceOut $     between (string "\"") (string "\"") (many1 (noneOf "\""))
                     <|>  between (string "'") (string "'") (many1 (noneOf "'"))
  return (attr, value)

many1Till :: Parser Char -> Parser [(String, String)] -> Parser String
many1Till p e = do
       notFollowedBy e
       x <- p
       xs <- manyTill p (try e)
       return (x:xs)

many1Till' :: Parser Char -> Parser [(String, String)] -> Parser (String, [(String, String)])
many1Till' p e = do
                 xs <- many1Till p (lookAhead e)
                 y <- e
                 return (xs, y)

openTag :: Parser (String, [(String, String)])
openTag = do
            string "<"
            skipMany space
            many1Till' anyChar $ do
                                   attrs <- spaceOut $ try (many attr)
                                   string ">"
                                   return attrs

closeTag :: String -> Parser String
closeTag str = do
                 _ <- string "</"
                 y <- string str
                 _ <- (string ">")
                 return y

sameConstructor :: Either a1 b1 -> Either a2 b2 -> Bool
sameConstructor (Left _) (Left _) = True
sameConstructor (Right _) (Right _) = True
sameConstructor _ _ = False
 
tagWithoutContent :: Parser Tag
tagWithoutContent = do
                      (x, a) <- tagWithoutContent'
                      return (Tag a x Nothing)

tagWithoutContent' :: Parser (String, [(String, String)])
tagWithoutContent' = do 
                       string "<"
                       skipMany space
                       many1Till' anyChar $ do
                                              attrs <- spaceOut $ try (many (try attr))
                                              string "/>"
                                              return attrs

groupEithers :: [Either a b] -> [Either [a] [b]]
groupEithers xs = map (\xs@(x:_) -> either (const (Left (lefts xs))) (const (Right (rights xs))) x) (groupBy sameConstructor xs)

closedTag = do
              (x,a) <- openTag
              let frag = fmap Left fragmentParse
              let text = fmap Right (noneOf "><" >>= return . (:[]) )
              y <- manyTill (try frag <|> text) (try $ closeTag x)
              let s = map (either head ( TagString . replaceEntities . concat )) (groupEithers y)
              return (Tag a x (Just s))
  
fragmentParse :: Parser Tag
fragmentParse = do
                  b <- closedTag
                  return b

xmlParser :: Parser Tag
xmlParser = do
              spaces
              x <- try tagWithoutContent <|> fragmentParse
              spaces
              eof
              return x

escapeCode :: Parser String
escapeCode = do
               char '&'
               x <- choice (map (\(k,v) -> string k >> return v ) escapeCodes)
               char ';'
               return [x]

replaceEntities :: String -> String
replaceEntities = (\(Right x) -> x) . parse (replaceParser escapeCode) ""

replaceParser :: Parser String -> Parser String
replaceParser parser = replaceParsers parser (anyChar >>=(return . (:[])))

replaceParsers :: Parser String -> Parser String -> Parser String
replaceParsers p1 p2 = do 
                         xs <- many (try p1 <|> p2)
                         return (concat xs)

parseXML :: String -> Either ErrMsg Tag
parseXML input = case parse xmlParser "" input of
                      Left err  -> Left $ show err
                      Right val -> Right val


parseFile :: FilePath -> IO (Either ErrMsg Tag)
parseFile fp = do s <- readFile fp; return $ parseXML s


-- Removes spaces
remSpace :: Parser a -> Parser a
remSpace p =
            do a <- p
               spaces
               return a


-----------
-- PLAN:
-- Atomer:
--     parser-atomer               =  erlang-atomer
--    [Ship_a|Service_a|Company_a] = [ship|service|company]
--    Derved kan der parses til variabelnavne
-- 
-- Variabelnavne
-- 
-- 

