import Data.Maybe

newtype NS = NS Symbol
type Offset = Integer

data Atom =
    AVar Symbol
    | AVarn Symbol
    | AConst Numeral
    | ATrue
    | AFalse
    | AQuote String
    | AString String
    | AFun NS TailForm
    | AFunC Symbol NS TailForm
     --- Uni Name Space
    | UFun Symbol TailForm
    | UFunC Symbol Symbol TailForm
    | UVar Symbol
     --- Nameless Variable
    | NLVar Offset


data TailForm =
--    | TAtom Atom
     TCond Atom TailForm TailForm
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
    | TLetRec [Symbol] [Atom] TailForm


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
    in cps (s n'') j (AFun (NS n'') (TCond (AVarn n'') b1' b2'))

cps n (SFun i body) K =
    let (body', n') = cps (s n) body (AVarn n)
    in (TApp K (AFunC i (NS n) body'), n')

cps n (SApp f x) K =
    let (ir, n') = (cps (s (s n)) x 
                        (AFun (NS (s n)) 
                            (TAppc (AVarn n) (AVarn (s n)) K))))
    in cps n' f (AFun (NS n) ir)
cps n (SLet x bind body) K =
    let (ir, n') = cps (s n) body K
    in cps n' bind (AFun (NS n) (TLet x (AVarn n) ir))

cps n (SLetRec fs xs bodies body) K =
    let lams = map (\(id', body') -> SFun id' body') (zip xs bodies)
        -- lams are all the functions declared in letrec
    in let (funs, n') = cpsalllam n lams
    in TLetRec fs funs (cps n' body K)
    where cpsalllam :: Assign -> [Exp] -> ([Atom], Assign)
          cpsalllam n lam:lams' = 
                let (TApp _ ft, n') = cps n lam ATrue
                in  let (fs, n'') = cpsalllam n' lams
                in (ft : fs, n'')
          cpsalllam n [] = ([], n)
        

cps n (SPair e1 e2) K =
    let (e2', n') = cps (s (s n)) e2 (AFun (NS (s n)) (EPair (AVarn n) (AVarn (s n)) K))
    in cps n' e1 (AFun (NS n) e2')

cps n (SCar e) K =
    cps (s n) e (AFun (NS n) (ECar (AVarn n) K))

cps n (SCdr e) K =
    cps (s n) e (AFun (NS n) (ECdr (AVarn n) K))

cps n (SQuote s) K = 
    (TApp K (AQuote s), n)
cps n (SString s) K = 
    (TApp K (AString s), n)

cps n (SAdd e1 e2) K =
    let (ir, n') = cps (s (s n)) e2 (AFun (NS (s n)) (EAdd (AVarn n) (AVarn (s n) K)))
    in cps n' e1 (AFun (NS n) ir)

cps n (SMult e1 e2) K =
    let (ir, n') = cps (s (s n)) e2 (AFun (NS (s n)) (EMult (AVarn n) (AVarn (s n) K)))
    in cps n' e1 (AFun (NS n) ir)

cps n (SNeg e) K =
    cps (s n) e (AFun (NS n) (ENeg (AVarn n) K))

cps n (SInv e) K =
    cps (s n) e (AFun (NS n) (EInv (AVarn n) K))
    
cps n (SBegin (x : [])) K =
    cps n x K

cps n (SBegin (x : y)) K =
    let (ir, n') = cps (s n) (SBegin y) K
    in cps n' x (AFun (NS n) ir)

cps n (SSys no e) =
    cps (s n) e (AFun (NS n) (ESys no (AVarn n) K))

uniVarspace :: TailForm -> Assign -> TailForm
uniVarspace (TCond a1 b1 b2) n = 
    TCond (unirename a1 n) (uniVarspace b1 n) (uniVarspace b2 n)
uniVarspace (TApp a1 a2) n = 
    TApp (unirename a1 n) (unirename a2 n)
uniVarspace (TAppc a1 a2 a3) n =
    TAppc (unirename a1 n) (unirename a2 n) (unirename a3 n)
uniVarspace (EAdd a1 a2 a3) n =
    EAdd (unirename a1 n) (unirename a2 n) (unirename a3 n)
uniVarspace (EMult a1 a2 a3) n =
    EMult (unirename a1 n) (unirename a2 n) (unirename a3 n)
uniVarspace (ENeg a1 a2) n =
    ENeg (unirename a1 n) (unirename a2 n)
uniVarspace (EInv a1 a2) n =
    EInv (unirename a1 n) (unirename a2 n)
uniVarspace (EPair a1 a2 a3) n =
    EPair (unirename a1 n) (unirename a2 n) (unirename a3 n)
uniVarspace (ECar a1 a2) n =
    ECar (unirename a1 n) (unirename a2 n)
uniVarspace (ECdr a1 a2) n =
    ECdr (unirename a1 n) (unirename a2 n)
uniVarspace (EZerop a1 a2) n =
    EZerop (unirename a1 n) (unirename a2 n)
uniVarspace (ESys k a1 a2) n =
    ESys k (unirename a1 n) (unirename a2 n)
uniVarspace (TLet i bind body) n =
    TLet i (unirename bind n) (uniVarspace body n)
uniVarspace (TLetRec fs binds body) n =
    let fs' = map (+ n) fs
        binds' = map (\x -> unirename x n) binds 
    in TLetRec fs' binds' (uniVarspace body n)  


unirename :: Atom -> Assign -> Atom
unirename (AVar i) n = UVar (i + n)
unirename (AVarn i) n = UVar i
unirename (AFun (NS i) body) n = UFun i (uniVarspace body n)
unirename (AFunC i (NS j) body) n = UFunC (i + n) j (uniVarspace body n)
unirename other _ = other

type Env = [Integer]

find :: Env -> Symbol -> Offset
find (v: ctx) target = if (v == target) 
                        then 0
                        else 1 + (find ctx target)
find [] target =  error "Wrong: Free Variable Found."

nameelia :: Env -> Atom -> Atom 
nameelia env (UFun i body) = UFun i (nameelit (i : env) body)
nameelia env (UFunC i j body) = UFunC i j (nameelit (j : i : env) body)
nameelia env (UVar i) = NLVar (find env i)

nameelit :: Env -> TailForm -> TailForm
nameelit env (TCond x y z) = 
    TCond (nameelia env x) (nameelit env y) (nameelit env z)
nameelit env (TApp x y) =
    TApp (nameelia env x) (nameelia y)
nameelit env (TAppc x y z) = 
    TAppc (nameelia env x) (nameelia env y) (nameelia z)
nameelit env (EAdd x y z) =
    EAdd (nameelia env x) (nameelia env y) (nameelia env z)
nameelit env (EMult x y z) =
    EMult (nameelia env x) (nameelia env y) (nameelia env z)
nameelit env (ENeg x y) =
    ENeg (nameelia env x) (nameelia env y) 
nameelit env (EInv x y) =
    EInv (nameelia env x) (nameelia env y)
nameelit env (EPair x y z) =
    EPair (nameelia env x) (nameelia env y) (nameelia env z)
nameelit env (ECar x y) =
    ECar (nameelia env x) (nameelia env y) 
nameelit env (ECdr x y) =
    ECdr (nameelia env x) (nameelia env y) 
nameelit env (EZerop x y) =
    EZerop (nameelia env x) (nameelia env y) 
nameelit env (ESys x y) =
    ESys x (nameelia env y) 
nameelit env (TLet i bind body) =
    TLet i (nameelia env bind) (nameelit (i: env) body)
nameelit env (TLetRec js fs body) =
    let newenv = (reverse js) ++ env 
    in let fs' = map (nameelia newenv) fs 
    in TLetRec js fs' (nameelit newenv body)