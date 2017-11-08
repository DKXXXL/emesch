module IRep where
import Frontend


data Atom =
    AVar Symbol
    | AVarn Symbol
    | AConst Numeral
    | ATrue
    | AFalse
    | AQuote String
    | AString String
    | AFun NS TailForm
    | AFunC Symbol NS TailForm
     --- Uni Name Space
    | UFun Symbol TailForm
    | UFunC Symbol Symbol TailForm
    | UVar Symbol
     --- Nameless Variable
    | NLVar Offset


data TailForm =
--    | TAtom Atom
     TCond Atom TailForm TailForm
    | TApp Atom Atom
    | TAppc Atom Atom Atom
    | EAdd Atom Atom Atom
    | EMult Atom Atom Atom
    | ENeg Atom Atom
    | EInv Atom Atom
    | EPair Atom Atom Atom
    | ECar Atom Atom
    | ECdr Atom Atom
    | EZerop Atom Atom
    | ESys Numeral Atom Atom
    | TLet Symbol Atom TailForm
    | TLetRec [Symbol] [Atom] TailForm


data InnerOperator =
        ADD 
        | MULT
        | INV
        | NEG
        | CAR
        | CDR
        | PAIR
        | ZEROP
        | SYS
        deriving Show
    
type Register = Integer
    
data Literal = 
        LNumber Numeral
        | LTrue
        | LFalse
        | LQuote String  
        | LString String
    
data MachL =
        SetEnvReg Offset Register
        | SetEnvEnv Offset Offset
        | SetRegEnv Register Offset
        | SetRegLabel Register Integer
        | SetRegLiteral Register Literal
        | LABEL Integer
        | Apply
        | ApplyInner InnerOperator
        | SaveCtxToEnv Offset 
        -- The variable at 'offset' is a closure
        | SaveCtxToReg Register 
        -- The value at 'register' is a closure, so that savectx is meaningful
        | AddEnv Integer
        | IfEnvLabel Offset Integer Integer
    
type LABELNO = Integer
newtype NS = NS Symbol
type Offset = Integer

