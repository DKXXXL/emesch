module Register(Register,ICop,Cdata) where

type Address = Int

data Cdata =
  CInt Int
  | CBool Bool
  | CString String
  | CQuote String
  | CAtom String
  | CLabel [ICop]
  | CExItem String Cdata


addcall :: String -> [String] -> String
addcall func (arg:args) = func ++ "(" ++ arg ++ (sepbyp args) ++ ")"
  where sepbyp args = concat . map (',':) $ args


instance (Show Cdata) where
  show (CString a) = "\"" ++ a ++ "\""
  show (CAtom a) = addcall "ATOM" [show (CString a)]
  show (CQuote a) = addcall "QUOTE" [show (CString a)]
  show (CInt a) = show a
  show (CBool a) = show a
  show (CExItem a _) = show a

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

    
data Register =
  Argl -- Pointer
  | Val -- Var
  deriving Show
    

