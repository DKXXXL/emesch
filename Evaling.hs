module Evaling where


import Internal
import Parsing

-- interpret -> eval -> apply

evalList :: [(Frames -> (Indata, Frames))] -> Frames -> (Indata, Frames)
evalList (thisf:[]) env = thisf env
evalList (thisf:toanalyz) env = evalList toanalyz (case thisf env of (_,frame) -> frame)

evalList' :: [(Frames -> (Indata, Frames))] -> Frames -> ([Indata], Frames)
evalList' (thisf:[]) env = case thisf env of (x,frame) -> (x:[],frame)
evalList' (thisf:toanalyz) env = let (x,frame) = thisf env
                                 in let (ret,frames) = evalList' toanalyz frame
                                    in (x:ret,frames)

--eval : analyze -> apply

eval :: SStruc -> (Frames -> (Indata, Frames))
eval (SString x) env =  (IString x, env)
eval (SNum x)  env  =  (INum x, env)
eval (SBool x) env =  (IBool x, env)
eval (SQuote x) env  = (IQuote x, env)
eval (SAtom x) env = (lookupFrames x, env)

--define
eval (SList ((SAtom "define"):(SAtom x):body:[])) env = (IQuote "()",addtoFrames env x (eval body)) 
eval (SList ((SAtom "define"):(SList ((SAtom funcName):args)):body:[])) env =
 eval (SList ((SAtom "define"):(SAtom funcName):(SList (SAtom "lambda"):(SList args):body))) env

--set

--let

--let*

--letrec

--continuation?

--lambda
eval (SList ((SAtom "lambda"):(args):body)) env =
  (IProcedure $ makeProcedure args body env , env)
  where makeProcedure :: [SAtom] -> [SStruc] -> (Frames -> [Indata] -> (Indata, Frames))
        makeProcedure formargs body frames actargs =
          let e' = evalList $ map eval body
          in (\(x,y) ->(x, backFrames y)) $ e' $ createNewFrame frames $ listzip formargs actargs
        listzip (x:x') (y:y') = (x,y):(listzip x' y')
        listzip [] [] = []
        

eval (SList (func:args))  = \env ->
  let (args', newenv) = evalList' (map eval args) env
  in let (IProcedure func') = (eval func newenv)
     in func' args
