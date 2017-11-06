import Data.Maybe

newtype NS = NS Symbol

data Atom =
    AVar Symbol
    | AVarn Symbol
    | AConst Numeral
    | ATrue
    | AFalse
    | AQuote String
    | AFun NS TailForm
    | AFunC Symbol NS TailForm


data TailForm =
    | TAtom Atom
    | TCond Atom TailForm TailForm
    | TApp Atom Atom
    | TAppc Atom Atom Atom
    | EAdd Atom Atom Atom
    | EMult Atom Atom Atom
    | ENeg Atom Atom
    | EInv Atom Atom
    | EPair Atom Atom Atom
    | ECar Atom Atom
    | ECdr Atom Atom
    | EZerop Atom Atom
    | ESys Numeral Atom Atom
    | TLet Symbol Atom TailForm
    | TLetRec Symbol Atom TailForm
    | TBegin [TailForm]

type Assign = Integer

s :: Assign -> Assign
s = (+ 1)

atomize :: Exp -> Maybe Atom
atomize (SConst n) = Just (AConst n)
atomize (STrue) = Just ATrue
atomize (SFalse) = Just AFalse 
atomize (SVar i) = Just (AVar i)
atomize (SQuote s) = Just (AQuote s)
atomize _ = Nothing

cps :: Assign -> Exp -> Atom -> (TailForm, Assign)
cps n (SConst n) K =
    (TApp K (AConst n), n)
cps n STrue K =
    (TApp K ATrue, n)
cps n SFalse K =
    (TApp K AFalse, n)

cps n (SVar i) K = 
    (TApp K (AVar i), n)
cps n (SZerop e) K =
    cps (s n) e (AFun (NS n) (EZerop (AVarn n) K))

cps n (SCond j b1 b2) K = 
   let (b1', n') = cps n b1 K
    in let (b2' , n'' ) = cps n' b2 K
    in cps (s n'') j (AFun (NS n'') (TCond a b1' b2'))

cps n (SFun i body) K =
    let (body', n') = cps (s n) body (AVarn n)
    in (TApp K (AFunC i (NS n) body'), n')

cps n (SApp f x) K =
    let (ir, n') = (cps (s (s n)) x 
                        (AFun (NS (s n)) 
                            (TAppc (AVarn n) (AVarn (s n)) K))))
    in cps n' f (AFun (NS n) ir)
cps n ()

