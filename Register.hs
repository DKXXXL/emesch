module Register(Register,ICop,Cdata) where

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
  deriving Show
  

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
  | VarCatch' Register Cdata Cdata Cdata Cdata
  | TestGo Register [ICop] [ICop]
  | LookVar Register Cdata
  | SetVar Cdata Register
  | DefVar Cdata Register
  | GetVar' Cdata Cdata Register
  | SetVar' Cdata Cdata Register
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
