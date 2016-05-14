module Register(Register,ICop,Cdata) where

type Address = Int

data Cdata =
  CInt Int
  | CBool Bool
  | CString String
  | CQuote String
  | CAtom String
  | CLabel [ICop]
  | CExItem String
data ICop =
  Assign1 Register Register
  | Assign2 Register Cdata 
  | Push Register Register
  | Pop Register Register
  | Pull Register Register
  | Run Cdata 
  | Call Register
  | TestGo Register Cdata Cdata
  | LookVar Register Cdata
  | SetVar Cdata Register
  | DefVar Cdata Register

    
data Register =
  Argl -- Pointer
  | Val -- Var
  deriving Show
    

