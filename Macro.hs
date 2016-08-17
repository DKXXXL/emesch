module Macro (macroTransformer) where
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
          $[beginTransformer,quoteTransformer]
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

beginTransformer (SList ((SAtom "begin"):x)) =
  (SList [SList ((SAtom "lambda"):(SList []):x)] ,True)

beginTransformer x = (x,False)
