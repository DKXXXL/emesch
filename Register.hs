module Register (internalFunc ,Register(..), ICop(..), ICi(..), Cdata(..), nameGenerator') where

type Address = Int

data Cdata =
  CInt Int
  | CBool Bool
  | CString String
--  | CQuote String
--  | CList [Cdata]
  | CAtom String
  | CLambda ICi  -- Ops, Register and variable in use
  | CExItem String
  deriving Eq

instance (Show Cdata) where
  show (CString a) = show a
--  show (CQuote a) = addcall "QUOTE" [show $ CString a]
  show (CInt a) = show a
  show (CBool a) = show a
  show (CExItem a) = a
  show (CLambda a) = show a
  show (CAtom a) = a

--data ICi =
--  ICi {ops :: [ICop], using :: [Register], var :: [Cdata]}

data ICi =
  ICi {ops :: [ICop],
       links :: [(Cdata,Cdata)] ,
       using :: [Register] ,
       var :: [Cdata],
       ref ::[Cdata]}
  deriving (Show, Eq)
-- operation is the operating list
-- linkages are the external variable
-- using means the registers in use
-- var means the variable relative to
--ICi' :: [ICop] -> [Register] -> ICi
--ICi' = \x y -> ICi x [] y

data ICop =
  Assign1 Register Register
  | Assign2 Register Cdata
  | Assign3 Register Cdata  -- Cdata = CLambda, construct lambda, catch variable then assign
  | Push Register Register
  | Pop Register Register
  | Label Cdata
  | Goto Cdata 
  | Call Register
  | Callc Register Cdata
  | CCall Register
  | Callb
  | VarCatch Register Cdata Cdata
  | VarCatch1 Register Cdata Cdata Cdata Cdata
  | VarCatch2 Register Cdata Cdata Cdata Cdata
  | TestGo Register [ICop] [ICop]
  | LookVar Register Cdata
  | SetVar Cdata Register
  | GetVar Cdata Register
  | SetVar1 Cdata Cdata Register
  | GetVar1 Cdata Cdata Register
  | DefVar Cdata Register
  | GetVar2 Cdata Cdata Register
  | SetVar2 Cdata Cdata Register
  | Save Register Register
  | Load Register Register
  deriving (Show, Eq)
data Register =
  Exp
  | Argl    -- Pointer
  | Val     -- Var
  | Env     -- Envrionment Stack
  | Ret     -- Return Stack
  deriving (Show, Eq)
    

nameGenerator' :: ICi -> String
nameGenerator'  = show

internalFunc :: [String]
internalFunc = ["cons","car","cdr","quote","+","-","*","/","begin"] 
