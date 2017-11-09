module Parsing2 (preparserToparenthesis,parser, parser', SStruc(..)) where


import Text.ParserCombinators.ReadP
import Data.List

data SStruc =
  SAtom String
  | SString String
  | SQuote SStruc
  | SList [SStruc]
  | SBool Bool
  | SNum Int
    deriving (Show, Eq)

oneOf :: [Char] -> ReadP Char
oneOf y = satisfy (\x -> x `elem` y) 

oneOf' :: [Char] -> ReadP [Char]
oneOf' y = munch (\x -> x `elem` y)

notOf :: [Char] -> ReadP Char
notOf y = satisfy (\x -> not (x `elem` y))

notOf' :: [Char] -> ReadP [Char]
notOf' y = munch (\x -> not (x `elem` y))


symbol'' = ",.;'[]-=<>?:\"{}|_+!@#$%^&*()~"
letter'' = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
number'' = "1234567890"
spaces'' = " \t\r\n"
numberandspaces'' = "1234567890 \t\r\n"
symbolandletter'' = ",.;'[]-=<>?:\"{}|_+!@#$%^&*()~"

symbol = oneOf symbol''
letter = oneOf letter''
number = oneOf number''

spaces = many1 . oneOf $ spaces''
optionalspaces = many . oneOf $ spaces''

parserAtom = do
  x <- (oneOf $ symbol'' ++ letter'')
  y <- (oneOf' $ symbol'' ++ letter'' ++ number'')
  case (x:y) of "#t" -> return $ SBool True
                "#f" -> return $ SBool False
                q -> return $ SAtom q

parserList = do
  (char '(')
  spaces
  x <- (sepBy parserExp spaces)
  spaces
  (char ')')
  return $ SList x

parserQuote = do
  (char '\'')
  optionalspaces
  x <- parserList <++ parserString <++ parserNumber <++ parserInvalidAtom
  return $ SQuote x
  where parserInvalidAtom =
          (many1 $ symbol <++ letter <++ number) >>=
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
parser' = readP_to_S (optionalspaces >> parserExp) 
parser :: String -> SStruc
parser = fst. head . parser' . (\x -> preparserToparenthesis x "")

preparserToparenthesis :: String -> String -> String
preparserToparenthesis ('(':xs) = cobs (\x -> " ( " ++ x) $ (preparserToparenthesis xs)
preparserToparenthesis (')':xs) = cobs (\x -> " ) " ++ x) $ (preparserToparenthesis xs)
preparserToparenthesis (x:xs) = cobs (\y -> [x] ++ y) $ (preparserToparenthesis xs)
preparserToparenthesis [] = \x -> "" ++ x
cobs :: (String -> String) -> (String -> String) -> (String -> String)
cobs f g = \x -> f.g $ x
{-  concat . map (\x -> case x of '(' -> " ( "
                                ')' -> " ) "
                                x' -> [x'])
 
  -}                              

---- It's too slow
