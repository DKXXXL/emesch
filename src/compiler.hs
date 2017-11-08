module Compiler where
import Data.Maybe
import IRep
import Frontend





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
cps n (SConst x) kont =
    (TApp kont (AConst x), n)
cps n STrue kont =
    (TApp kont ATrue, n)
cps n SFalse kont =
    (TApp kont AFalse, n)

cps n (SVar i) kont = 
    (TApp kont (AVar i), n)
cps n (SZerop e) kont =
    cps (s n) e (AFun (NS n) (EZerop (AVarn n) kont))

cps n (SCond j b1 b2) kont = 
   let (b1', n') = cps n b1 kont
    in let (b2' , n'' ) = cps n' b2 kont
    in cps (s n'') j (AFun (NS n'') (TCond (AVarn n'') b1' b2'))

cps n (SFun i body) kont =
    let (body', n') = cps (s n) body (AVarn n)
    in (TApp kont (AFunC i (NS n) body'), n')

cps n (SApp f x) kont =
    let (ir, n') = (cps (s (s n)) x 
                        (AFun (NS (s n)) 
                            (TAppc (AVarn n) (AVarn (s n)) kont)))
    in cps n' f (AFun (NS n) ir)
cps n (SLet x bind body) kont =
    let (ir, n') = cps (s n) body kont
    in cps n' bind (AFun (NS n) (TLet x (AVarn n) ir))

cps n (SLetRec fs xs bodies body) kont =
    let lams = map (\(id', body') -> SFun id' body') (zip xs bodies)
        -- lams are all the functions declared in letrec
    in let (funs, n') = cpsalllam n lams
    in let (body', n'') = (cps n' body kont)
    in (TLetRec fs funs body', n'')
    where cpsalllam :: Assign -> [Exp] -> ([Atom], Assign)
          cpsalllam n (lam:lams') = 
                let (TApp _ ft, n') = cps n lam ATrue
                in  let (fs, n'') = cpsalllam n' lams'
                in (ft : fs, n'')
          cpsalllam n [] = ([], n)
        

cps n (SPair e1 e2) kont =
    let (e2', n') = cps (s (s n)) e2 (AFun (NS (s n)) (EPair (AVarn n) (AVarn (s n)) kont))
    in cps n' e1 (AFun (NS n) e2')

cps n (SCar e) kont =
    cps (s n) e (AFun (NS n) (ECar (AVarn n) kont))

cps n (SCdr e) kont =
    cps (s n) e (AFun (NS n) (ECdr (AVarn n) kont))

cps n (SQuote s) kont = 
    (TApp kont (AQuote s), n)
cps n (SString s) kont = 
    (TApp kont (AString s), n)

cps n (SAdd e1 e2) kont =
    let (ir, n') = cps (s (s n)) e2 (AFun (NS (s n)) (EAdd (AVarn n) (AVarn (s n)) kont))
    in cps n' e1 (AFun (NS n) ir)

cps n (SMult e1 e2) kont =
    let (ir, n') = cps (s (s n)) e2 (AFun (NS (s n)) (EMult (AVarn n) (AVarn (s n)) kont))
    in cps n' e1 (AFun (NS n) ir)

cps n (SNeg e) kont =
    cps (s n) e (AFun (NS n) (ENeg (AVarn n) kont))

cps n (SInv e) kont =
    cps (s n) e (AFun (NS n) (EInv (AVarn n) kont))
    
cps n (SBegin (x : [])) kont =
    cps n x kont

cps n (SBegin (x : y)) kont =
    let (ir, n') = cps (s n) (SBegin y) kont
    in cps n' x (AFun (NS n) ir)

cps n (SSys no e) kont =
    cps (s n) e (AFun (NS n) (ESys no (AVarn n) kont))



cpsOf_ :: Exp -> TailForm
cpsOf_ e = let (ret, _) = cps 2 e (AFun (NS 1) (ESys (-1) (AVarn 1) (AConst 0))) in ret

unifyVariableName_ :: TailForm -> TailForm
unifyVariableName_ t = uniVarspace t 1

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

nameElimination_ :: TailForm -> TailForm
nameElimination_ = nameelit []

nameelia :: Env -> Atom -> Atom 
nameelia env (UFun i body) = UFun i (nameelit (i : env) body)
nameelia env (UFunC i j body) = UFunC i j (nameelit (j : i : env) body)
nameelia env (UVar i) = NLVar (find env i)
nameelia _ x = x

nameelit :: Env -> TailForm -> TailForm
nameelit env (TCond x y z) = 
    TCond (nameelia env x) (nameelit env y) (nameelit env z)
nameelit env (TApp x y) =
    TApp (nameelia env x) (nameelia env y)
nameelit env (TAppc x y z) = 
    TAppc (nameelia env x) (nameelia env y) (nameelia env z)
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
nameelit env (ESys x y z) =
    ESys x (nameelia env y) z
nameelit env (TLet i bind body) =
    TLet i (nameelia env bind) (nameelit (i: env) body)
nameelit env (TLetRec js fs body) =
    let newenv = (reverse js) ++ env 
    in let fs' = map (nameelia newenv) fs 
    in TLetRec js fs' (nameelit newenv body)