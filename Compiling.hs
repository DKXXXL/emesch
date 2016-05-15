import Register
import Internal

import Data.List (nub, concat)

randomname :: String -> String

data ICi = ICi {operation :: ICop, linkages :: [(Cdata,Cdata)] ,using :: [Register]}


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
  where opt1 :: [ICop] -> [ICop]
--        opt1 ((Run (CLabel x)):rem) = (opt1 x) ++ (opt1 rem)
--        opt1 ((Assign2 y (CLabel x)):rem) = (Assign2 y (CLabel (opt1 x))):(opt1 rem) 
--        opt1 (x:rem) = x : (opt1 rem)
        integrateICi :: [ICi] -> ICi
        integrateICi icis =
          ICi (Run (CLabel (map . operation $ icis))) (concat. map. linkages $ icis)
          (nub. concat. map. using $ icis) 

        
compile :: SStruc -> ICi
compile (SString x) =ICi (Assign2 Val (CString x)) [] [Val]
compile (SNum x) =ICi (Assign2 Val (CInt x)) [] [Val]
compile (SBool x) =ICi (Assign2 Val (CBool x)) [] [Val]
compile (SQuote x) =ICi (Assign2 Val (CQuote x)) [] [Val]
compile (SAtom x) =ICi (LookVar Val (CAtom x)) [] [Val]

compile (SList ((SAtom "define"):(SAtom x):body:[])) =
  (ICi (Run (CLabel [(compile body), 
                     DefVar (CAtom x) Val])) [] [Val])
                        
compile (SList ((SAtom "define"):(SList ((SAtom funcName):args)):body:[])) =
  compile (SList ((SAtom "define"):(SAtom funcName):(SList (SAtom "lambda"):(SList args):body)))


compile (SList ((SAtom "lambda"):(SList arg):body))@all =
  let name = nameGenerator all
  in ICi
     (Assign2 Val (CExItem name))
     [(CExItem name,
       CLabel (compileLambda' ((SList (reverse arg)):body)))]
     [Val]
  where compileLambda' :: [SStruc] -> ICop 
        compileLambda' (SList ((SAtom arg): args):body) =
          Run (CLabel [(Pop Argl Val),
                       (DefVar (CAtom arg) Val),
                       (compileLambda' ((SList args):body))])
        compileLambda' ((SList []):body) =
          Run (CLabel (compileList body))

compile (SList ((SAtom "if"):pred:branch1:branch2)) =
  ICi (Run (CLabel [(compile pred),
                    (TestGo Val (CLabel (compileList branch1))
                     (CLabel (compileList branch2)))])) [] [Val]

compile (SList ((SAtom "set!"):(SAtom var):val:[])) =
  ICi (Run (CLabel [(compile val),
                    (SetVar (CAtom var) Val)])) [] [Val]

compile (SList (func:arg:args)) =
  ICi (Run (CLabel [(compile arg),
                    (Push Argl Val),
                    (compile (SList (func:args)))])) [] [Argl]


compile (SList ((SAtom "cons"):[])) = ICi (Run (CLabel [Assign2 Val (CExItem "CONS"),
                                                        Call Val])) [] [Val]

compile (SList ((SAtom "car"):[])) = ICi (Run (CLabel [Assign2 Val (CExItem "CAR"),
                                                       Call Val])) [] [Val]

compile (SList ((SAtom "cdr"):[])) = ICi (Run (CLabel [Assign2 Val (CExItem "CDR"),
                                                       Call Val])) [] [Val]



compile (SList (func:[])) =
  ICi (Run (CLabel [(compile func),
                    (Call Val)])) [] [Val]



addcall :: String -> [String] -> String
addcall func (arg:args) = func ++ "(" ++ arg ++ (sepbyp args) ++ ")"
  where sepbyp [] = ""
        sepbyp args = concat . map (',':) $ args


assignmentsentence :: String -> String -> String
assignmentsentence to from = to ++ "=" ++ from ++ ";"



optoC :: ICop -> String
optoC (Run (CLabel x)) = concat . map optoC $ x
optoC (Assign1 a b) = assignmentsentence (show a) (addcall "(ptlong)" [show b])
optoC (Assign2 a b) = assignmentsentence (show a) (addcall "(ptlong)" [show b])
optoC (Push a b) =
  assignmentsentence
  (addcall "*" [addcall "(ptlong*)" [(show a) ++ "++"]])
  (show b)











