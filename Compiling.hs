import Register
import Internal

data ICi = ICi {operation :: ICop, linkages :: [(Cdata, Cdata)], using :: [Register]}


compileList :: [SStruc] -> [ICop]
compileList = map compile
  where opt1 :: [ICop] -> [ICop]
        opt1 ((Run (CLabel x)):rem) = (opt1 x) ++ (opt1 rem)
        opt1 ((Assign2 y (CLabel x)):rem) = (Assign2 y (CLabel (opt1 x))):(opt1 rem) 
        opt1 (x:rem) = x : (opt1 rem)

compile :: SStruc -> ICi
compile (SString x) = Assign2 Val (CString x)
compile (SNum x) = Assign2 Val (CInt x)
compile (SBool x) = Assign2 Val (CBool x)
compile (SQuote x) = Assign2 Val (CQuote x)
compile (SAtom x) = LookVar Val (CAtom x)

compile (SList ((SAtom "define"):(SAtom x):body:[])) =
  Run (CLabel [(compile body), 
               DefVar (CAtom x) Val])  
compile (SList ((SAtom "define"):(SList ((SAtom funcName):args)):body:[])) =
 compile (SList ((SAtom "define"):(SAtom funcName):(SList (SAtom "lambda"):(SList args):body)))


compile (SList ((SAtom "lambda"):argandbody)) =
  Assign2 Val (CLabel (compileLambda' argandbody))
  where compileLambda' :: [SStruc] -> ICop 
        compileLambda' (SList ((SAtom arg): args):body) =
          Run (CLabel [(Pull Argl Val),
                       (DefVar (CAtom arg) Val),
                       (compileLambda' ((SList args):body))])
        compileLambda' ((SList []):body) =
          Run (CLabel (compileList body))


compile (SList ((SAtom "cons"):a:b:[])) =
  Run (CLabel [(compile a),
               (Push Argl Val),
               (compile b),
               (Push Argl Val),
               (compile (SAtom "CONS")),
               (Call Val)])
  
compile (SList ((SAtom "car"):a:[])) =
  Run (CLabel [(compile a),
               (Push Argl Val),
               (compile (SAtom "CAR")),
               (Call Val)])

compile (SList ((SAtom "cdr"):a:[])) =
  Run (CLabel [(compile a),
               (Push Argl Val),
               (compile (SAtom "CDR")),
               (Call Val)])

compile (SList ((SAtom "if"):pred:branch1:branch2)) =
  Run (CLabel [(compile pred),
               (TestGo Val (Label (compileList branch1))
                (Label (compileList branch2)))])

compile (SList ((SAtom "set!"):(SAtom var):val:[])) =
  Run (CLabel [(compile val),
               (SetVar (CAtom var) Val)])

compile (SList (func:arg:args)) = Run (CLabel [(compile arg),
                                               (Push Argl Val),
                                               (compile (SList (func:args)))])
  
compile (SList (func:[])) = Run (CLabel [(compile func),
                                         (Call Val)])



addCast :: String -> String ->String
addCast cast var = "(" ++ (cast ++ (")" ++ var))

addParenthesis :: String -> String
addParenthesis a = '(' : (a ++ ")")


toC :: ICop -> String


toC' (Assign1 a b) = (show a) ++ ("=" ++ (show b))
toC' (Assign2 a b) = (show a) ++ ("=" ++ (addCast "ptlong" (show b)))
toC' (Push ra rb) =
  "*" ++ (addParenthesis (addParenthesis (addCast "ptlong*" (show ra))) ++ "++") ++ "=" ++(show rb)
toC' (Pull ra rb) =  
