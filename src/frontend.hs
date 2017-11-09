module Frontend(Symbol, Numeral, Exp(..)) where

--- AST
type Symbol = Integer
type Numeral = Integer

data Exp =
    SConst Numeral
    | STrue
    | SFalse
    | SVar Symbol
    | SZerop Exp
    | SCond Exp Exp Exp 
    | SFun Symbol Exp 
    | SApp Exp Exp
    | SLet Symbol Exp Exp 
    | SLetRec [Symbol] [Symbol] [Exp] Exp 
    | SPair Exp Exp 
    | SCar Exp 
    | SCdr Exp
    | SQuote String
    | SString String
    | SAdd Exp Exp 
    | SMult Exp Exp 
    | SNeg Exp 
    | SInv Exp
    | SBegin [Exp]
    | SSys Numeral Exp 
    deriving Show



