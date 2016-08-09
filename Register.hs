module Register (Register(..), ICop(..), ICi(..), Cdata(..), nameGenerator') where

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
  

instance (Show Cdata) where
  show (CString a) = "\"" ++ a ++ "\""
--  show (CQuote a) = addcall "QUOTE" [show $ CString a]
  show (CInt a) = show a
  show (CBool a) = show a
  show (CExItem a) = show a
  show (CLambda a) = show a
  show (CAtom a) = show a

--data ICi =
--  ICi {ops :: [ICop], using :: [Register], var :: [Cdata]}

data ICi =
  ICi {ops :: [ICop],
       links :: [(Cdata,Cdata)] ,
       using :: [Register] ,
       var :: [Cdata],
       refvar ::[Cdata]}
  deriving Show
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
  | CCall Register Cdata
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
  | Save Register
  | Load Register
  deriving Show
data Register =
  Exp
  | Argl    -- Pointer
  | Val     -- Var
  | Env     -- Envrionment Stack
  | Ret     -- Return Stack
  deriving Show
    

nameGenerator' :: ICi -> String
nameGenerator'  = show
