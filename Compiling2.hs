

-------------To IR

compile :: SStruc -> ICi

compile (SString x) =ICi [Assign2 Val (CString x)][Val] []
compile (SNum x) =ICi [Assign2 Val (CInt x)] [Val] []
compile (SBool x) =ICi [Assign2 Val (CBool x)] [Val] []
compile (SAtom x) =ICi [LookVar Val (CAtom x)] [Val] [CAtom x]

compile (SList ((SAtom "define"):(SList ((SAtom funcName):args)):body:[])) =
  compile (SList ((SAtom "define"):(SAtom funcName):(SList (SAtom "lambda"):(SList args):body)))


compile (SList ((SAtom "define"):(SAtom x):body:[])) =
  acobC $
  (compile body):
  (ICi [DefVar (CAtom x) Val] [Val] [CAtom x]):[]


compile (SList ((SAtom "lambda"):(SList arg):body))@all =
  let lambdai' = acobC [compileLambdaEntrance arg,
                        compileBody body]
  in  ICi [Assign2 Val (CLambda lambdai')] [Val] [] 
  where compileBody = acobC . map compile
        compileLambdaEntrance :: [SStruc] -> ICop
        compileLambdaEntrance = concat . map compileLambdaEntranceArg . reverse
          where compileLambdaEntranceArg (SAtom arg) =
                  [(Pop Argl Val),
                   (DefVar (CAtom arg) Val)]

compile (SList ((SAtom "if"):pred:branch1:branch2)) =
  acobC [(compile pred),
         ICi [(TestGo Val (CLabel (compileList branch1))),
              (CLabel (compileList branch2))] [Val] []]

compile (SList ((SAtom "set!"):(SAtom var):val:[])) =

  acobC [(compile val),
         ICi [(SetVar (CAtom var) Val)] [Val] [CAtom var]]

{-
compile (SList ((SAtom "call/cc"):x:[]))@x' =
  let name = nameGenerator x'
      name' = "GOTO" ++ name 
  in ICi [Save Argl,
          Assign2 Val (CExItem (name')),
          Push Argl Val,
          (compile x),
          Call Val,
          Label (CExItem name)]
     [(name',CLabel $ ICi [Pop Argl Val,
                           Load Argl,
                           Goto (CExitem name)] [] [Argl,Val])]
     [Argl,Val]
     []

-}

compile (SList (func:args)) =
  acobC [(compile func),
         (ICi [Push Exp Val] [Exp,Val] []),
         (compileArgs args)]
  where compileArgs :: [SStruc] -> ICi
        compileArgs =
          acobC . concat $
          map (\x -> [(compile x),
                      (ICi [Push Argl Val] [Argl,Val] [])])
  

acobC :: [ICi] -> ICi
acobC = ICi
  (concat . map . ops $ icis)
  (nub . concat . map . using $ icis) 
  (nub . concat . map . var $ icis) 

-------------To C

nameGenerator :: [SStruc] -> String
nameGenerator = concat . map nameGenerator'
where nameGenerator' :: SStruc -> String
      nameGenerator' (SAtom x) = x
      nameGenerator' (SString x) = x
      nameGenerator' (SQuote x) = nameGenerator' x
      nameGenerator' (SList l) = nameGenerator l
      nameGenerator' (SBool x) = show x
      nameGenerator' (SNum x) = show x

