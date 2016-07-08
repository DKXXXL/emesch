import Register
import Internal
import CPattern

import Data.List (nub, concat, find)
import Data.Char (toUpper)


-------------------------------------------------------------------------------------
-- 1. Cdata have a pointer long data ahead, because it has to store the class of data
-------------------------------------------------------------------------------------

allcompile :: (ICi -> ICi) -> [SStruc] -> String
allcompile opt =
  addheader "runtime.h" .
  addheader "emeschlib.h" .
  linkagetoC .
  (\x ->(CExItem "main",CLabel . opt . lexAddr $ x)) .
  envSet .
  compileList .
  (map macroTransformer)


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
          (nub . concat . map . using $ icis) 
          (nub . concat . map . var $ icis) 
        

cobC :: ICi -> ICi -> ICi
cobC a b =
  ICi ((++) (operation a) (operation b))
  ((++) (linkages a) (linkages b))
  (nub ((++) (using a) (using b)))
  (nub ((++) (var a) (var b)))

compile :: SStruc -> ICi
compile (SString x) =ICi [Assign2 Val (CString x)] [] [Val] []
compile (SNum x) =ICi [Assign2 Val (CInt x)] [] [Val] []
compile (SBool x) =ICi [Assign2 Val (CBool x)] [] [Val] []

compile (SAtom x) =ICi [LookVar Val (CAtom x)] [] [Val] [CAtom x]
{-
compile (SQuote (SAtom x)) = compile (SList [SAtom "quote", SString x])

compile (SQuote (SString x)) = compile (SList [SAtom "quote", SString x])
compile (SQuote (SList x)) = compile (SList [SAtom "quote",(SList x)])

compile (SQuote (SList (x:[]))) =
  compile (SList [SAtom "cons",
                  SQuote x,
                  SQuote $ SString "()"])

compile (SList ((SAtom "quote"):(SList x:y:z):[])) =
  compile (SList [SAtom "cons",
                  SQuote x,
                  SQuote (SList (y:z))])  
-}

compile (SList ((SAtom "define"):(SList ((SAtom funcName):args)):body:[])) =
  compile (SList ((SAtom "define"):(SAtom funcName):(SList (SAtom "lambda"):(SList args):body)))

compile (SList ((SAtom "define"):(SAtom x):body:[])) =
  (compile body)
  `cobC`
  (ICi [DefVar (CAtom x) Val] [] [Val] [CAtom x])

compile (SList ((SAtom "lambda"):(SList arg):body))@all =
  let name = nameGenerator all
      lambda' = compileLambda ((SList (reverse arg)):body) [] [Val,Argl]
  in ICi
     [(Assign2 Val (CExItem name))]
     [(CExItem name,CLabel lambda')]
     [Val]
  where compileLambda :: [SStruc] -> [(Cdata,Cdata)] -> [Register] -> ICi
        compileLambda (args:body) links regs =
          let body' = compileList body
          in
            ICi
            ((compileLambdaEntrance args) ++ (operation body'))
            ((linkages body') ++ links)
            ((using body') ++ regs)
            (var body')
        compileLambdaEntrance :: [SStruc] -> [ICop]
        compileLambdaEntrance (SList ((SAtom arg): args)) =
          [(Pop Argl Val),
           (DefVar (CAtom arg) Val)] ++
          (compileLambdaEntrance ((SList args):body))
        compileLambdaEntrance (SList []) = []
      

compile (SList ((SAtom "if"):pred:branch1:branch2)) =
  (compile pred)
  `cobC`
  $ ICi [(TestGo Val (CLabel (compileList branch1))
          (CLabel (compileList branch2)))] [] [Val] []

compile (SList ((SAtom "set!"):(SAtom var):val:[])) =
  (compile val)
  `cobC`
  $ ICi [(SetVar (CAtom var) Val)] [] [Val] [CAtom var]

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

compile (SList (func:args)) =
  (compile func)
  `cobC`
  (ICi
   [(Push Exp Val)]
   []
   [Exp,Argl]
   [])
  `cobC`
  (compileArgs args)
  `cobC`
  (ICi
   [(Pop Exp Val),
    (Call Val)]
   []
   [Exp,Val]
   [])
  where compileArgs :: [SStruc] -> ICi
        compileArgs (arg:l) =
          (compile arg)
          `cobC`
          (ICi
           [Push Argl Val]
           []
           [Argl]
           [])
          `cobC` $ compileArgs l
        compileArgs [] = ICi [] [] [] []
  



{-
compile (SList ((SAtom "cons"):[])) = ICi [Assign2 Val (CExItem "CONS"),
                                            Call Val] [] [Val]

compile (SList ((SAtom "car"):[])) = ICi [Assign2 Val (CExItem "CAR"),
                                          Call Val] [] [Val]

compile (SList ((SAtom "cdr"):[])) = ICi [Assign2 Val (CExItem "CDR"),
                                          Call Val] [] [Val]

compile (SList (func:[])) =
  ICi [(compile func),
       (Call Val)] [] [Val]
-}

envSet :: ICi -> ICi
envSet (ICi ops x y z) = ICi (envload' ++ ops) x y z
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
{-
lexAddr :: ICi -> ICi
lexAddr = (\((ICi a b c),_,i) -> ICi a b ((LexVec i):c)) . (\x -> lexaddr (x, [[]], 0)) 
where lexaddr' :: (ICop,Table,Int) -> (ICop,Table,Int)
      lexaddr' (DefVar cd r, t, i) = (SetLVec (CInt i) r, addaddr' t (cd,i), i+1)
      lexaddr' (SetVar cd r, t, i) = (SetLVec (CInt $ lookaddr' t cd) r, t , i)
      lexaddr' (LookVar r cd, t, i) = (GetLVec (CInt $ lookaddr' t cd) r, t, i)
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
                     
-}

instance (Show Cdata) where
  show (CString a) = "\"" ++ a ++ "\""
--  show (CQuote a) = addcall "QUOTE" [show $ CString a]
  show (CInt a) = show a
  show (CBool a) = show a
  show (CExItem a) = show a
--  show (CAtom a) = addcall "ATOM" [show (CString a)]

  -- show (CLabel x) =  cube . concat . map optoC $ x




icitoC :: ICi -> String -> String
icitoC (ICi ops linkages regs vars) funcname =
  (concat . map regtoC $ regs)
  ++ (concat . map linkagetoC $ linkages)
  ++ (declfun funcname (concat . map optoC $ ops) $ map show vars)




optoC :: ICop -> String


optoC (Label a) = (show a) ++ ":"
optoC (Goto a) = sentence $ "goto " ++ (show a) 
optoC (Save r) = sentence $ addcall "SAVE" [show r]
optoC (Load r) = sentence $ addcall "LOAD" [show r]

--optoC (Run (CLabel x)) = concat . map optoC $ x
optoC (Assign1 a b) = assignmentsentence (show a) (addcall "(ptlong)" [show b])
--optoC (Assign2 a b) = assignmentsentence (show a) (addcall "(ptlong)" [show b])
optoC (Assign2 a cd) =sentence $ addcall "ASSIGN2" [show $ a,
                                                    datatype cd,
                                                    show cd]
  where datatype (CInt _) = "1"
        datatype (CString _) = "2"
        datatype (CBool _) = "3"
optoC (Push a b) =
  assignmentsentence
  (addcall "*" [addcall "(ptlong*)" [(show a) ++ "++"]])
  (addcall "(ptlong)" [(show b)])

optoC (Pop a b) =
  assignmentsentence
  (show b)
  (addcall "(ptlong)" [addcall "(*)" [addcall "--" [show b]]])

optoC (Call a) =
  sentence $ addcall (addcall "" [addcall "((void*)())"  [show a]]) []

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


optoC (GetLVec (CInt cd) r) =
  assignmentsentence
  (show r)
  (addcall "*" [addcall "(ptlong*)" [offsetof "LexVec" cd]])
optoC (SetLVec (CInt cd) r) =
  assignmentsentence
  (addcall "*" [addcall "(ptlong*)" [offsetof "LexVec" cd]])
  (show r)

linkagetoC (CExItem a,CLabel b) = icitoC b a
linkagetoC (CExItem a,b) = declvar a $ show b


regtoC (LexVec i) = declarray "LexVec" i
regtoC (x) = declvar x . show . CInt $ 0 
