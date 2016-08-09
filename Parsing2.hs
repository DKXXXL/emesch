module Parsing2 (parser, SStruc(..)) where


import Text.ParserCombinators.ReadP
import Data.List

data SStruc =
  SAtom String
  | SString String
  | SQuote SStruc
  | SList [SStruc]
  | SBool Bool
  | SNum Int
    deriving Show


oneOf :: [Char] -> ReadP Char
oneOf (x:y) = foldl' (\x y -> x +++ (char y)) (char x) y

symbol = oneOf ",.;'[]-=<>?:\"{}|_+!@#$%^&*()~"
letter = oneOf "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
number = oneOf "1234567890"
spaces = many1 . oneOf $ " \t\r\n"

parserAtom = do
  x <- (symbol +++ letter)
  y <- (many $ (symbol +++ letter +++ number))
  case (x:y) of "#t" -> return $ SBool True
                "#f" -> return $ SBool False
                q -> return $ SAtom q

parserList = do
  (char '(')
  (optional spaces)
  x <- (sepBy1 parserExp spaces)
  (optional spaces)
  (char ')')
  return $ SList x

parserQuote = do
  (char '\'') 
  x <- parserList <++ parserString <++ parserNumber <++ parserInvalidAtom
  return $ SQuote x
  where parserInvalidAtom =
          (many1 $ symbol +++ letter +++ number) >>=
          \x -> return $ SAtom x

parserString = do
  (char '\"')
  x <- munch (\x -> x /= '\"')
  (char '\"')
  return $ SString x

parserNumber = do
  x <- many1 $ number
  return . SNum $ read x

parserExp = parserList <++ parserQuote <++ parserString <++ parserNumber <++ parserAtom

parser' :: String -> [(SStruc,String)]
parser' = readP_to_S parserExp
parser :: String -> SStruc
parser = (\((x,y):z) -> x) . parser'
