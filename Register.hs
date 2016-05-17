module Register(Register,ICop,Cdata) where

type Address = Int

data Cdata =
  CInt Int
  | CBool Bool
  | CString String
--  | CQuote String
--  | CList [Cdata]
  | CAtom String
  | CLabel ICi
  | CExItem String
    

data ICi = ICi {operation :: [ICop], linkages :: [(Cdata,Cdata)] ,using :: [Register]}
ICi' :: [ICop] -> [Register] -> ICi
ICi' = \x y -> ICi x [] y

data ICop =
  Assign1 Register Register
  | Assign2 Register Cdata 
  | Push Register Register
  | Pop Register Register
  | Run Cdata 
  | Call Register
  | TestGo Register Cdata Cdata
  | LookVar Register Cdata
  | SetVar Cdata Register
  | DefVar Cdata Register
  | GetVec Cdata Register
  | SetVec Cdata Register
    
data Register =
  Argl -- Pointer
  | Val -- Var
  deriving Show
    

