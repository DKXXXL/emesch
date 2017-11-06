module FrontEnd where

--- AST

type Symbol = Int
type Numeral = Int

data Exp =
    SConst Numeral
    | STrue
    | SFalse
    | SVar Symbol
    | SZeropSL Exp
    | SCond Exp Exp Exp 
    | SFun Symbol Exp 
    | SApp Exp Exp
    | SLet Symbol Exp Exp 
    | SLetRec Symbol Exp Exp 
    | SPair Exp Exp 
    | SCar Exp 
    | SCdr Exp
    | SQuote String
    | SAdd Exp Exp 
    | SMult Exp Exp 
    | SNeg Exp 
    | SInv Exp Exp 
    | SBegin [Exp]
    | SSys Numeral Exp 



