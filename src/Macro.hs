module Macro (macroTransformer, carefulOperator) where
import Parsing2 
import Data.List (foldl')

type MacroT = SStruc -> (SStruc,Bool)

macroTransformer :: SStruc -> SStruc
macroTransformer = falsethenend allT
  where falsethenend t = \x -> case t x of (x', False) -> x'
                                           (x', True) -> falsethenend t x'
        allT = makeallT allT'
        everywhereT :: MacroT -> MacroT
        everywhereT t = everywhereT' `combineTs` t
          where everywhereT' :: MacroT
                everywhereT' (SList x) =
                  let res :: [(SStruc,Bool)]
                      res = map (everywhereT' `combineTs` t) x
                  in (SList $ map fst res,
                      foldl' (||) False $ map snd res)
                everywhereT' x = t x
        makeallT :: [MacroT] -> (MacroT)
        makeallT (x:y) = foldl' combineTs x y
        combineTs t1 t2 = (\x ->
                            let (x' ,res1) = t2 x
                            in case res1 of False -> t1 x'
                                            True -> case t1 x' of (x'',_) -> (x'', True))
        allT' =
          map everywhereT
          --          $ [quoteTransformer,beginTransformer]
          $[{-beginTransformer,-}quoteTransformer, appcurried, lamcurried, eliMinus, eliDivide, binaried]
quoteTransformer :: MacroT
quoteTransformer (SQuote (SNum x)) =
  (SNum x,True)

quoteTransformer (SQuote (SAtom x)) =
  ((SList [SAtom "quote", SString x]),True)

quoteTransformer (SQuote (SString x)) =
  ((SList [SAtom "quote", SString x]),True)


quoteTransformer (SList (((SAtom "quote"):(SList (x:y:z)):[]))) =
  ((SList [SAtom "cons",
           SQuote x,
           SQuote (SList (y:z))]),True)

quoteTransformer (SQuote (SList [])) =
  (SQuote $ SString "()", True)

quoteTransformer (SQuote (SList (x:[]))) =
  ((SList [SAtom "cons",
           SQuote x,
           SQuote $ SString "()"]),True)

quoteTransformer (SQuote (SList (x:y:z))) =
  ((SList [SAtom "cons",
           SQuote x,
           SQuote (SList (y:z))]), True)


quoteTransformer x = (x,False)

{-
beginTransformer (SList ((SAtom "begin"):x)) =
  (SList [SList ((SAtom "lambda"):(SList []):x)] ,True)

beginTransformer x = (x,False)
-}

binOps  = [SAtom "+",  SAtom "*"]

isin :: Eq a => a -> [a] -> Bool
isin x (y:z) = (x == y) || (isin x z)
isin x [] = False

intOps = binOps ++ [ SAtom "/", SAtom "-",SAtom "lambda", 
    SAtom "quote", SAtom "cons", SAtom "if", SAtom "neg", SAtom "inv",
      SAtom "begin", SAtom "zerop", SAtom "let", 
      SAtom "letrec", SAtom "car", SAtom "cdr"]

carefulOperator :: SStruc -> Bool 
carefulOperator x = x `isin` intOps

binOperator :: SStruc -> Bool
binOperator x = x `isin` binOps

appcurried :: MacroT
appcurried (SList (x:y:[])) = 
  (SList (x:y:[]), False)

appcurried (SList (x:y:z)) = 
  if (carefulOperator x) 
    then (SList (x:y:z), False)
    else (SList (SList (x:y:[]):z), True)

appcurried x = (x, False)

lamcurried :: MacroT
lamcurried (SList ((SAtom "lambda"): (SList (x:y:z)): body)) =
  ((SList [SAtom "lambda", 

            (SList (x:[])), 

            SList ([SAtom "lambda", 
                    SList (y:z)] ++ body)] ), True)

lamcurried x = (x, False)

eliMinus :: MacroT 
eliMinus (SList ((SAtom "-"):x:z)) =
  let tominus = map (\y -> (SList ((SAtom "neg"):y:[]))) z
  in (SList ((SAtom "+"):x:tominus), True)

eliMinus x = (x, False)

eliDivide :: MacroT
eliDivide (SList ((SAtom "/"):x:z)) =
  let tominus = map (\y -> (SList ((SAtom "inv"):y:[]))) z
  in (SList ((SAtom "*"):x:tominus), True)

eliDivide x = (x, False)

binaried :: MacroT
binaried (SList (x:y:z:[])) =
  ((SList (x:y:z:[])), False)
binaried (SList (x:y:z:a)) =
  if (binOperator x) 
    then (SList (x:y:(SList (x:z:a):[])), True) 
    else (SList (x:y:z:a), False)

binaried x = (x, False)

