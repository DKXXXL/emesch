import Register
import Internal
import CPattern

import Data.List (nub, concat, find)
import Data.Char (toUpper)


-------------------------------------------------------------------------------------
-- 1. Cdata have a pointer long data ahead, because it has to store the class of data
-------------------------------------------------------------------------------------

----------- To IR

acobC :: [ICi] -> ICi
acobC icis = ICi
  (concat . map . ops $ icis)
  (concat . map . links $ icis)
  (nub . concat . map . using $ icis) 
  (nub . concat . map . var $ icis) 


compile :: SStruc -> ICi
compile (SString x) =ICi [Assign2 Val (CString x)] [] [Val] []
compile (SNum x) =ICi [Assign2 Val (CInt x)] [] [Val] []
compile (SBool x) =ICi [Assign2 Val (CBool x)] [] [Val] []

compile (SAtom x) =ICi [LookVar Val (CAtom x)] [] [Val] [CAtom x]

compile (SList ((SAtom "define"):(SList ((SAtom funcName):args)):body:[])) =
  compile (SList ((SAtom "define"):(SAtom funcName):(SList (SAtom "lambda"):(SList args):body)))


compile (SList ((SAtom "define"):(SAtom x):body:[])) =
  acobC $
  (compile body):
  (ICi [DefVar (CAtom x) Val] [] [Val] [CAtom x]):[]


compile (SList ((SAtom "lambda"):(SList arg):body))@all =
  let lambdai' = acobC [compileLambdaEntrance arg,
                        compileBody body]
      lname = nameGenerator all
  in  ICi ((Assign2 Val (CExItem lname)) : varcatchLambda) 
      [(CExItem lname,CLambda lambdai')] [Val] [] 
  where compileBody = acobC . map compile
        compileLambdaEntrance :: [SStruc] -> ICop
        compileLambdaEntrance = concat . map compileLambdaEntranceArg . reverse
          where compileLambdaEntranceArg (SAtom arg) =
                  [(Pop Argl Val),
                   (DefVar (CAtom arg) Val)]
        varcatchLambda :: [ICop]
        varcatchLambda = map (\x -> VarCatch Val x (CExItem lname)) $ map CAtom $ var lambdai'

compile (SList ((SAtom "if"):pred:branch1:branch2)) =
  let b1 = nameGenerator branch1
      b2 = nameGenerator branch2
  in acobC [(compile pred),
            ICi [(TestGo
                  Val
                  [Assign2 Val (CExItem b1),
                   Call Val]
                  [Assign2 Val (CExItem b2),
                   Call Val])]
            [(CExItem b1,CLambda $ compileList branch1)
             (CExItem b2,Clambda $ compileList branch2)] [Val] []]
  where compileList = acobC . map compile

compile (SList ((SAtom "set!"):(SAtom var):val:[])) =
  acobC [(compile val),
         ICi [(SetVar (CAtom var) Val)] [] [Val] [CAtom var]]


compile (SList ((SAtom "call/cc"):x:[]))@x' =
  undefined


compile (SList (func:args)) =
  acobC [(compile func),
         (ICi [Push Exp Val] [] [Exp,Val] []),
         (compileArgs args),
         (ICi [Pop Exp Val,
               Run Val] [] [Exp,Val] [])]
  where compileArgs :: [SStruc] -> ICi
        compileArgs =
          acobC . concat $
          map (\x -> [(compile x),
                      (ICi [Push Argl Val] [Argl,Val] [])])



nameGenerator :: [SStruc] -> String
nameGenerator = concat . map nameGenerator'
where nameGenerator' :: SStruc -> String
      nameGenerator' (SAtom x) = x
      nameGenerator' (SString x) = x
      nameGenerator' (SQuote x) = nameGenerator' x
      nameGenerator' (SList l) = nameGenerator l
      nameGenerator' (SBool x) = show x
      nameGenerator' (SNum x) = show x


------- Environment 

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


---------- TO C


allcompile :: (ICi -> ICi) -> [SStruc] -> String
allcompile opt =
  addheader "runtime.h" .
  addheader "emeschlib.h" .
  linkagetoC .
  (\x ->(CExItem "main",CLambda x)) .
  opt.
  envSet .
  compileList .
  (map macroTransformer)
  where compileList = acobC . map compile


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
