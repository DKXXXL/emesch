import Register
import Internal
import CPattern

import Data.List (nub, concat, find)
import Data.Char (toUpper)

allcompile :: (ICi -> ICi) -> [SStruc] -> String
allcompile opt =
  addheader "runtime.h" .
  addheader "emeschlib.h" .
  linkagetoC .
  (\x ->(CExItem "main",CLabel . opt. lexAddr $ x)) .
  envSet .
  compileList 



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

compile (SAtom x) =ICi [LookVar Val (CAtom x)] [] [Val]

compile (SQuote (SAtom x)) = compile (SList [SAtom "quote", SString x])

compile (SQuote (SString x)) = compile (SList [SAtom "quote", SString x])
compile (SQuote (SList x)) = compile (SList ((SAtom "quote"):x))

compile (SQuote (SList (x:[]))) =
  compile (SList [SAtom "cons",
                  SQuote x,
                  SQuote $ SString "()"])

compile (SList ((SAtom "quote"):x:y:z)) =
  compile (SList [SAtom "cons",
                  SQuote x,
                  SQuote (SList (y:z))])
  

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

{-
compile (SList ((SAtom "cons"):[])) = ICi [Assign2 Val (CExItem "CONS"),
                                            Call Val] [] [Val]

compile (SList ((SAtom "car"):[])) = ICi [Assign2 Val (CExItem "CAR"),
                                          Call Val] [] [Val]

compile (SList ((SAtom "cdr"):[])) = ICi [Assign2 Val (CExItem "CDR"),
                                          Call Val] [] [Val]
-}
compile (SList (func:[])) =
  ICi [(compile func),
       (Call Val)] [] [Val]

envSet :: ICi -> ICi
envSet (ICi ops x y) = ICi (envload' ++ ops) x y
  where envload' :: [ICop]
        envload' =
          concat . map envloadgen' $
          ["cons","car","cdr","quote","+","-","*","/"] 
        envloadgen' internalfunc = [Assign2 Val (CExItem $ envfuncalias internalfunc),
                                    DefVar (CAtom internalfunc) Val]
        where envfuncalias "+" = "ADD"
              envfuncalias "-" = "MINUS"
              envfuncalias "*" = "MUTIPLY"
              envfuncalias "/" = "DEVIDE"
              envfuncalias = map . toUpper
type Table = [[(String,Int)]] 

lexAddr :: ICi -> ICi
lexAddr = (\(x,_,_) -> x) . (\x -> lexaddr (x, [[]], 0)) 
where lexaddr' :: (ICop,Table,Int) -> (ICop,Table,Int)
      lexaddr' (DefVar cd r, t, i) = (SetVec (CInt i) r, addaddr' t (cd,i), i+1)
      lexaddr' (SetVar cd r, t, i) = (SetVec (CInt $ lookaddr' t cd) r, t , i)
      lexaddr' (LookupVar r cd, t, i) = (GetVec (CInt $ lookaddr' t cd) r, t, i)
      lexaddr' (x, t, i) = (x, t, i)
      lexaddr (ICi ops links regs, t, i) =
        foldl lexaddr'acc  (proclinks links (ICi [] [] [], addframe' t ,i)) ops
        where lexaddr'acc (_, t, i) op = lexaddr' (op, t, i)
              lexaddracc (_, t, i) ici = lexaddr (ici, addframe' . backframe' $ t , i)
              proclinks links basic =
                foldl lexaddracc basic.
                map (\x -> case x of (_,CLabel y) -> y
                                     (_,_) -> ICI [] [] []) $ links
              addframe' :: Table -> Table
              addframe' = ([]:)
              backframe' :: Table -> Table
              backframe' (_:x) = x
              addaddr' (y:l) x = (x:y):l
              lookaddr' t cd =foldl maybeacc Nothing . map (find (\(x,_) -> x == cd)) $ t
                where maybeacc (Just x) _ = x
                      maybeacc _ (Just x) = x
                      --if maybeacc miss something, then error 
                     
instance (Show Cdata) where
  show (CString a) = "\"" ++ a ++ "\""
--  show (CQuote a) = addcall "QUOTE" [show $ CString a]
  show (CInt a) = show a
  show (CBool a) = show a
  show (CExItem a) = show a
  show (CAtom a) = addcall "ATOM" [show (CString a)]

  -- show (CLabel x) =  cube . concat . map optoC $ x




icitoC :: ICi -> String -> String
icitoC (ICi ops linkages regs) funcname =
  (concat . map regtoC $ regs)
  ++ (concat . map linkagetoC $ linkages)
  ++ (declfun funcname $ (concat . map optoC $ ops))
  

optoC :: ICop -> String
--optoC (Run (CLabel x)) = concat . map optoC $ x
optoC (Assign1 a b) = assignmentsentence (show a) (addcall "(ptlong)" [show b])
--optoC (Assign2 a b) = assignmentsentence (show a) (addcall "(ptlong)" [show b])
optoC (Assign2 a cd) = addcall "ASSIGN2" [quotesentence . show $ a,
                                          show cd]
optoC (Push a b) =
  assignmentsentence
  (addcall "*" [addcall "(ptlong*)" [(show a) ++ "++"]])
  (add call "(ptlong)" [(show b)])

optoC (Pop a b) =
  assignmentsentence
  (show b)
  (addcall "(ptlong)" [addcall "(*)" [addcall "--" [show b]]])

optoC (Call a) =
  sentence $ addcall (addcall "" [addcall "((void*)())" [addcall "*(ptlong*)" [show a]]]) []

optoC (TestGo pred branch1 branch2) =
  ifsentence (show pred) (show branch1) (show branch2)
{-
optoC (LookVar a b) =
  addcall "LOOKVAR" [quotesentence . show $ a,
                     show b]

optoC (SetVar a b) =
  addcall "SETVAR" [show a,
                    quotesentence . show $ b]


optoC (DefVar a b) =
  addcall "SETVAR" [show a,
                    quotesentence . show $ b]
-}


optoC (GetVec cd r) = addcall "GETVEC" [show cd,
                                        quotesentence . show $ r]
optoC (SetVec cd r) = addcall "SETVEC" [show cd,
                                        quotesentence . show $ r]

linkagetoC (CExItem a,CLabel b) = icitoC b a
linkagetoC (CExItem a,b) = declvar a $ show b


regtoC (Register x) = declvar x . show . CInt $ 0 
