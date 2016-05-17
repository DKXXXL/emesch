module Parsing where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Internal 

symbol :: Parser Char
symbol = oneOf "~!@#$%^&*_+:?`;,./-="

spaces :: Parser String
spaces = many $ oneof " \t"

parserAtom' :: Parser SStruc
parserAtom' =do x <- (symbol <|> letter) 
                y <- (many $ symbol <|> letter <|> digit)
                return $ case (x : y) of "#t" -> SBool True
                                         "#f" -> SBool False
parserList :: Parser SStruc
parserList =do char '('
               x <- (parserExp `sepBy` spaces)
               char ')'
               return $ SList x

parserString :: Parser SStruc
parserString =do char '\"'
                 x <- noneOf '\"'
                 char '\"'
                 return $ SString x

parserQuote :: Parser SStruc
parserQuote =do char '\''
                x <- (parserAtom' <|> parserList <|> parserString)
                return $ SQuote x 

parserNumber :: Parser SStruc
parserNumber = many1 digit >>= (return . SNum . read)

parserExp :: Parser SStruc
parserExp = parserAtom' <|> parserList <|> parserString <|> parserQuote


type SAtom = String
data SStruc =
  SAtom SAtom
  | SString String
  | SQuote SStruc
  | SList [SStruc]
  | SBool Bool
  | SNum Int
    deriving Show

data InOp =
  Define SAtom SAtom
  | Lambda SList SList
  | Set SAtom SAtom
    

