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
    
  

--data ICi =
--  ICi {ops :: [ICop], using :: [Register], var :: [Cdata]}

data ICi =
  ICi {ops :: [ICop], links :: [(Cdata,Cdata)] ,using :: [Register] ,var :: [Cdata]}
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
  | VarCatch Register Cdata Cdata
  | VarCatch' Register Cdata Cdata Cdata
  | TestGo Register [ICop] [ICop]
  | LookVar Register Cdata
  | SetVar Cdata Register
  | DefVar Cdata Register
  | GetLVec Cdata Cdata Register
  | SetLVec Cdata Cdata Register
  | Save Register
  | Load Register
data Register =
  Exp
  | Argl -- Pointer
  | Val -- Var
  | LexVec Int
  deriving Show
    

