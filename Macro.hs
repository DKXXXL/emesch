import Parsing
module Macro (macroTransformer) where

module Macro(macroTransformer)
type MacroT = SStruc -> (SStruc,Bool)

macroTransformer :: SStruc -> SStruc
macroTransformer = falsethenend allT
  where falsethenend t = \x -> case t x of (x', False) -> x'
                                           (x', True) -> falsethenend t x'
        allT = makeallT allT'
        everywhereT :: MacroT -> MacroT
        everywhereT t = everywhereT'
          where everywhereT' (SList x) =
                  let res = map (everywhereT' `combineTs` t) x
                  in (SList . concat $ map (\(x,_) -> x) res,
                      foldl' or False $ map (\(_,x)->x) res)
                everywhereT' x = t x
        makeallT :: [MacroT] -> (MacroT)
        makeallT (x:y) = foldl' combineTs x y
        combineTs t1 t2 = (\x ->
                            let (x' ,res1) = t2 x
                            in case res1 of False -> t1 x'
                                            True -> case t1 x' of (x'',_) -> (x'', True))
        allT' =
          map everywhereT
          $ [quoteTransformer]
        
quoteTransformer :: MacroT
quoteTransformer (SQuote (SAtom x)) =
  ((SList [SAtom "quote", SString x]),True)

quoteTransformer (SQuote (SString x)) =
  ((SList [SAtom "quote", SString x]),True)

quoteTransformer (SQuote (SList x)) =
  ((SList [SAtom "quote",(SList x)]),True)

quoteTransformer (SQuote (SList (x:[]))) =
  ((SList [SAtom "cons",
           SQuote x,
           SQuote $ SString "()"]),True)

quoteTransformer (SList ((SAtom "quote"):(SList x:y:z):[])) =
  ((SList [SAtom "cons",
           SQuote x,
           SQuote (SList (y:z))]),True)

quoteTransformer x = (x,False)

