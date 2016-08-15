module Compiling (
  allcompile
--  ,compiletest
  ) where

import Macro
import NextCompiling
import Register
import CPattern
import Parsing2
import Data.List (nub, concat, find)
import Data.Char (toUpper)


-------------------------------------------------------------------------------------
-- 1. Cdata have a pointer long data ahead, because it has to store the class of data
-------------------------------------------------------------------------------------

----------- To IR

acobC :: [ICi] -> ICi
acobC icis =
  ICi
  (concat . map ops $ icis)
  (concat . map links $ icis) 
  (nub . concat . map  using $ icis)
  (nub . concat . map  var $ icis)
  (nub . concat . map  ref $ icis)

compile :: SStruc -> ICi
compile (SString x) =ICi [Assign2 Val (CString x)] [] [Val] [] []
compile (SNum x) =ICi [Assign2 Val (CInt x)] [] [Val] [] []
compile (SBool x) =ICi [Assign2 Val (CBool x)] [] [Val] [] []

compile (SAtom x) =ICi [LookVar Val (CAtom x)] [] [Val] [CAtom x] []

compile (SList ((SAtom "define"):(SList (funcName:args)):body)) =
  compile (SList [(SAtom "define"),(funcName),(SList ((SAtom "lambda"):(SList args):body))])


compile (SList ((SAtom "define"):(SAtom x):body:[])) =
  acobC $
  [(compile body),
   (ICi [DefVar (CAtom x) Val] [] [Val] [CAtom x] [])]


compile all@(SList ((SAtom "lambda"):(SList arg):body)) =
  let lambdai' = acobC [compileLambdaEntrance arg,
                        compileBody body]
      lname = nameGenerator all
  in  ICi ((Assign3 Val (CExItem lname)) : varcatchLambda) 
      [(CExItem lname,CLambda lambdai')] ([Val]) [] []
  where compileBody = acobC . map compile
        compileLambdaEntrance :: [SStruc] -> ICi
        compileLambdaEntrance = acobC . map compileLambdaEntranceArg . reverse
          where compileLambdaEntranceArg (SAtom arg) =
                  ICi [(Pop Argl Val),
                       (DefVar (CAtom arg) Val)] [] [Argl,Val] [(CAtom arg)] []
--        varcatchLambda :: [ICop]
--        varcatchLambda = map (\x -> VarCatch Val x (CExItem lname)) $ map CAtom $ var lambdai'
        varcatchLambda = []
compile (SList ((SAtom "if"):pred:branch1:branch2:[])) =
  let b1 = nameGenerator branch1
      b2 = nameGenerator branch2
  in acobC [(compile pred),
            ICi [(TestGo
                  Val
                  [Assign3 Val (CExItem b1),
                   Call Val] 
                  [Assign3 Val (CExItem b2),
                   Call Val])]
            [(CExItem b1,CLambda $ compile branch1),
             (CExItem b2,CLambda $ compile branch2)] [Val] [] []]
  where compileList = acobC . map compile

compile (SList ((SAtom "set!"):(SAtom var):val:[])) =
  acobC [(compile val),
         ICi [(SetVar (CAtom var) Val)] [] [Val] [CAtom var] []]


compile x'@(SList ((SAtom "call/cc"):x:[])) =
  let name = nameGenerator x
  in (acobC [ ICi [Save Env Val,
                   DefVar (CAtom "__env") Val,
                   Save Argl Val,
                   DefVar (CAtom "__argl") Val,
                   Save Exp Val,
                   DefVar (CAtom "__exp") Val,
                   Save Ret Val,
                   DefVar (CAtom "__ret") Val
                   ] [] [Argl,Val,Exp] [(CAtom "__env"),
                                        (CAtom "__argl"),
                                        (CAtom "__exp"),
                                        (CAtom "__ret")] [],
              
              compile x,
              
              ICi [
--                Push Exp Val,
--                   Assign3 Val (CExItem name),
--                   Push Argl Val,
--                   Pop Exp Val,
                CCall Val]
              []
              [Exp,Val,Argl]
              [] []
            ])




compile (SList (func:args)) =
  acobC [(compile $ possibleinFunc func),
         (ICi [Push Exp Val] [] [Exp,Val] [] []),
         (compileArgs args),
         (ICi [Pop Exp Val,
               Call Val] [] [Exp,Val] [] [])]
  where compileArgs :: [SStruc] -> ICi
        compileArgs =
          acobC . concat .
          map (\x -> [(compile x),
                      (ICi [Push Argl Val] [] [Argl,Val] [] [])])

        possibleinFunc (SAtom x) = case x `elem` internalFunc of True -> SAtom $ envfuncalias x
                                                                 False -> SAtom x
        



--compile' :: (SStruc,(Int,Int)) -> (ICi,(Int,Int))




nameGenerator :: SStruc -> String
nameGenerator = nameGenerator'
  where nameGenerator' :: SStruc -> String
        nameGenerator' (SAtom x) = x
        nameGenerator' (SString x) = x
        nameGenerator' (SQuote x) = nameGenerator' x
        nameGenerator' (SList l) = concat $ map nameGenerator' l
        nameGenerator' (SBool x) = show x
        nameGenerator' (SNum x) = show x




------- Environment 

envSet :: ICi -> ICi
envSet (ICi ops x y z e) =
  ICi (envload' ++ ops) x y (z ++ (map (CAtom . envfuncalias) internalFunc)) e
  where envload' :: [ICop]
        envload' =
          concat . map envloadgen' $ internalFunc
        envloadgen' internalfunc = [Assign2 Val (CExItem $ envfuncalias internalfunc),
                                    DefVar (CAtom $ envfuncalias internalfunc) Val] 
envfuncalias "+" = "ADD"
envfuncalias "-" = "MINUS"
envfuncalias "*" = "MUTIPLY"
envfuncalias "/" = "DEVIDE"
envfuncalias x = map toUpper x



---------- TO C


allcompile :: (ICi -> ICi) -> SStruc -> String
allcompile opt =
  addheader "runtime.h" .
  addheader "emeschlib.h" .
  linkagetoC .
  (\x ->(CExItem "main",CLambda x)) .
  opt.
  necessaryTransform .
  envSet .
  compile .
  macroTransformer
--  where compileList (SList x) = acobC . map compile $ x
   --     compileList x = compile x
{-
compiletest opt = 
  necessaryTransform .
  envSet .
  compile .
  macroTransformer
-}
--  where compileList (SList x) = acobC . map compile $ x          
{-
instance (Show Cdata) where
  show (CString a) = "\"" ++ a ++ "\""
--  show (CQuote a) = addcall "QUOTE" [show $ CString a]
  show (CInt a) = show a
  show (CBool a) = show a
  show (CExItem a) = show a
--  show (CAtom a) = addcall "ATOM" [show (CString a)]

  -- show (CLabel x) =  cube . concat . map optoC $ x

-}


icitoC :: ICi -> String -> String
icitoC (ICi ops linkages regs vars refs) funcname =
  (concat . map regtoC $  regs)
  ++ (concat . map linkagetoC $ linkages)
  ++ (declfunc funcname (concat . map optoC $ ops) $ map show vars)




optoC :: ICop -> String

--optoC (Run (CLabel x)) = concat . map optoC $ x
optoC (Assign1 a b) = assignmentsentence (show a) (addcall "(ptlong)" [show b])
--optoC (Assign2 a b) = assignmentsentence (show a) (addcall "(ptlong)" [show b])
optoC (Assign2 a cd) =sentence $ addcall ("ASSIGN2" ++ (datatype cd)) [show $ a,
                                                                       show cd]
  where datatype (CInt _) = "1"
        datatype (CString _) = "2"
        datatype (CBool _) = "3"
        datatype (CExItem _) = "4"
        datatype x = "0"
        
optoC (Push a b) =
  assignmentsentence
  (addcall "*" [addcall "(ptlong*)" [(show a) ++ "++"]])
  (addcall "(ptlong)" [(show b)])

optoC (Pop a b) =
  assignmentsentence
  (show b)
  (addcall "(ptlong)" [addcall "(*)" [addcall "--" [show b]]])

optoC (Label a) = (show a) ++ ":"
optoC (Goto a) = sentence $ "goto " ++ (show a) 



optoC (Callc a (CExItem x)) =
  --  sentence $ addcall (addcall "" [addcall "((void*)())"  [show a]]) []
  sentence $ addcall "CALL" [show a, instName x]

optoC (Callb) =
  sentence $ addcall "RETURN" []


optoC (VarCatch1 r x y name cla) =
  sentence $ addcall "VARCATCH" [show r, show x, show y, show name, show cla]


optoC (VarCatch2 r x y name cla) =
  sentence $ addcall "VARCATCHREF" [show r, show x, show y, show name, show cla]


optoC (TestGo pred branch1 branch2) =
  ifsentence (show pred)
  (foldr (++) ""  (map optoC branch1))
  (foldr (++) ""  (map optoC branch2))


optoC (SetVar1 x y r) =
  sentence $ addcall "SETVAR" [show x, show y, show r]


optoC (SetVar2 x y r) =
  sentence $ addcall "SETVARREF" [show x, show y, show r]


optoC (GetVar1 x y r) =
  sentence $ addcall "GETVAR" [show x, show y, show r]


optoC (GetVar2 x y r) =
  sentence $ addcall "GETVARREF" [show x, show y, show r]

optoC (Save r r') = sentence $ addcall "SAVE" [show r, show r']
optoC (Load r r') = sentence $ addcall "LOAD" [show r, show r']


{-
optoC (GetLVec (CInt cd) r) =
  assignmentsentence
  (show r)
  (addcall "*" [addcall "(ptlong*)" [offsetof "LexVec" cd]])
optoC (SetLVec (CInt cd) r) =
  assignmentsentence
  (addcall "*" [addcall "(ptlong*)" [offsetof "LexVec" cd]])
  (show r)
-}
linkagetoC (CExItem a,CLambda b) = icitoC b a
linkagetoC (CExItem a,b) = declvar a $ show b

{-
regtoC (LexVec i) = declarray "LexVec" i
-}

regSize :: Int
regSize = 128

regtoC (x) =
  (declarray (tr $ show x) regSize) ++
  (assignmentsentence (staticsentence $ pointertype ptlongtype $ show x) (tr $ show x))
  where tr = ('b':)
