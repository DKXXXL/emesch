import Register
import Internal

import Data.List (nub, concat)


allcompile :: [SStruc] -> String
allcompile = linkagetoC . \x ->(CExItem "main",CLabel x) . compileList


nameGenerator :: [SStruc] -> String
nameGenerator = concat . map nameGenerator'
where nameGenerator' :: SStruc -> String
      nameGenerator' (SAtom x) = x
      nameGenerator' (SString x) = x
      nameGenerator' (SQuote x) = nameGenerator' x
      nameGenerator' (SList l) = nameGenerator l
      nameGenerator' (SBool x) = show x
      nameGenerator' (SNum x) = show x

compileList :: [SStruc] -> ICi
compileList =integrateICi . map compile
  where integrateICi :: [ICi] -> ICi
        integrateICi icis =
          ICi
          (concat . map . operation $ icis)
          (concat . map . linkages $ icis)
          (nub . concat . map. using $ icis) 
         
        
compile :: SStruc -> ICi
compile (SString x) =ICi [Assign2 Val (CString x)] [] [Val]
compile (SNum x) =ICi [Assign2 Val (CInt x)] [] [Val]
compile (SBool x) =ICi [Assign2 Val (CBool x)] [] [Val]
compile (SQuote x) =ICi [Assign2 Val (CQuote x)] [] [Val]
compile (SAtom x) =ICi [LookVar Val (CAtom x)] [] [Val]

                        
compile (SList ((SAtom "define"):(SList ((SAtom funcName):args)):body:[])) =
  compile (SList ((SAtom "define"):(SAtom funcName):(SList (SAtom "lambda"):(SList args):body)))

compile (SList ((SAtom "define"):(SAtom x):body:[])) =
  (ICi  [(compile body), 
         DefVar (CAtom x) Val] [] [Val])

compile (SList ((SAtom "lambda"):(SList arg):body))@all =
  let name = nameGenerator all
      lambda' = ICi (compileLambda' ((SList (reverse arg)):body)) [] [Val,Argl]
  in ICi
     [(Assign2 Val (CExItem name))]
     [(CExItem name,CLabel lambda')]
     [Val]
  where compileLambda' :: [SStruc] -> [ICop] 
        compileLambda' (SList ((SAtom arg): args):body) =
          [(Pop Argl Val),
           (DefVar (CAtom arg) Val)] ++
          (compileLambda' ((SList args):body))  
        compileLambda' ((SList []):body) =
          [compileList body]

compile (SList ((SAtom "if"):pred:branch1:branch2)) =
  ICi [(compile pred),
       (TestGo Val (CLabel (compileList branch1))
        (CLabel (compileList branch2)))] [] [Val]

compile (SList ((SAtom "set!"):(SAtom var):val:[])) =
  ICi [(compile val),
       (SetVar (CAtom var) Val)] [] [Val]

compile (SList (func:arg:args)) =
  ICi [(compile arg),
       (Push Argl Val),
       (compile (SList (func:args)))] [] [Argl]


compile (SList ((SAtom "cons"):[])) = ICi [Assign2 Val (CExItem "CONS"),
                                            Call Val] [] [Val]

compile (SList ((SAtom "car"):[])) = ICi [Assign2 Val (CExItem "CAR"),
                                          Call Val] [] [Val]

compile (SList ((SAtom "cdr"):[])) = ICi [Assign2 Val (CExItem "CDR"),
                                          Call Val] [] [Val]



compile (SList (func:[])) =
  ICi [(compile func),
       (Call Val)] [] [Val]



instance (Show Cdata) where
  show (CString a) = "\"" ++ a ++ "\""
  show (CAtom a) = addcall "ATOM" [show (CString a)]
  show (CQuote a) = addcall "QUOTE" [show (CString a)]
  show (CInt a) = show a
  show (CBool a) = show a
  show (CExItem a) = show a
 -- show (CLabel x) =  cube . concat . map optoC $ x



    
addcall :: String -> [String] -> String
addcall func (arg:args) = func ++ "(" ++ arg ++ (sepbyp args) ++ ")"
  where sepbyp [] = ""
        sepbyp args = concat . map (',':) $ args


assignmentsentence :: String -> String -> String
assignmentsentence to from = to ++ "=" ++ from ++ ";"

sentence:: String -> String
sentence = (++ ";")

cube :: String -> String
cube x = ('{' : x) ++ "}" 

quotesentence ::String -> String
quotesentence = '&':
ifsentence :: String -> String -> String -> String
ifsentence pred branch1 branch2 =
  "if" ++ (addcall "" [pred]) ++ (branch1) ++ "else" ++ (branch2)

declfunc :: String -> String -> String
declfunc funcname funcbody = "int " ++ (addcall funcname []) ++ (cube funcbody) 

declvar :: String -> String -> String
declvar name val = "ptlong " ++ name ++ "=" ++ val ++ ";"

icitoC :: ICi -> String -> String
icitoC (ICi ops linkages regs) funcname =
  (concat . map regtoC $ regs)
  ++ (concat . map linkagetoC $ linkages)
  ++ (declfun funcname $ (concat . map optoC $ ops))
  

optoC :: ICop -> String
optoC (Run (CLabel x)) = concat . map optoC $ x
optoC (Assign1 a b) = assignmentsentence (show a) (addcall "(ptlong)" [show b])
optoC (Assign2 a b) = assignmentsentence (show a) (addcall "(ptlong)" [show b])
optoC (Push a b) =
  assignmentsentence
  (addcall "*" [addcall "(ptlong*)" [(show a) ++ "++"]])
  (add call "(ptlong)" [(show b)])

optoC (Pop a b) =
  assignmentsentence
  (show b)
  (addcall "(ptlong)" [addcall "(*)" [addcall "--" [show b]]])

optoC (Call a) =
  sentence $ addcall (addcall "" [addcall "((void*)())" [show a]]) []

optoC (TestGo pred branch1 branch2) =
  ifsentence (show pred) (show branch1) (show branch2)

optoC (LookVar a b) =
  addcall "LOOKVAR" [quotesentence . show $ a,
                     show b]

optoC (SetVar a b) =
  addcall "SETVAR" [show a,
                    quotesentence . show $ b]


optoC (DefVar a b) =
  addcall "SETVAR" [show a,
                    quotesentence . show $ b]

linkagetoC (CExItem a,CLabel b) = icitoC b a
linkagetoC (CExItem a,b) = declvar a $ show b


regtoC (Register x) = declvar x . show . CInt $ 0 
