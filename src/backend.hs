data InnerOperator =
    ADD 
    | MULT
    | INV
    | NEG
    | CAR
    | CDR
    | PAIR
    | ZEROP
    | SYS

type Register = Integer

data Literal = 
    LNumber Numeral
    | LTrue
    | LFalse
    | LQuote String  
    | LString String

data MachL =
    SetEnvReg Offset Register
    | SetEnvEnv Offset Offset
    | SetRegEnv Register Offset
    | SetEnvLabel Offset Integer
    | SetRegLabel Register Integer
    | SetRegLiteral Register Literal
    | LABEL Integer
    | Apply
    | ApplyInner InnerOperator
    | SaveCtxToEnv Offset 
    -- The variable at 'offset' is a closure
    | SaveCtxToReg Register 
    -- The value at 'register' is a closure, so that savectx is meaningful
    | AddEnv Integer
    | IfEnvLabel Offset Integer Integer

type LABELNO = Integer

s = (+ 1)

mapValueToRegister :: LABELNO -> [Atom] -> [Register] -> ([MachL], [MachL], LABELNO)
mapValueToRegister n (a:as) (r: rs) =
    let (running, funs, n') = initValueToRegister n a r
    in let (afrunning, affuns, n'') = mapValueToRegister n' as rs
    in (running + afrunning, funs + affuns, n'')
mapValueToRegister n [] _ =
    ([], [], n)

initValueToRegister :: LABELNO -> Atom -> Register -> ([MachL], [MachL], LABELNO)
initValueToRegister n (AConst i) r =
    ([SetRegLiteral r (LNumber i)], [], n)

initValueToRegister n ATrue r =
    ([SetRegLiteral r LTrue], [], n)
initValueToRegister n AFalse r =
    ([SetRegLiteral r LFalse], [], n)
initValueToRegister n (AQuote s) r =
    ([SetRegLiteral r (LQuote s))], [], n)
initValueToRegister n (AString s) r =
    ([SetRegLiteral r (LString s))], [], n)

initValueToRegister n (NLVar i) r =
    ([SetRegEnv r i], [], n)
initValueToRegister n (UFun i body) r =
    let (comfun, n') = backend (s n) body
    in (
        [SetRegLabel r n, 
        SaveCtxToReg r],
        ((LABEL n) : 
         (AddEnv 1) :
         (SetEnvReg 0 1) :
        comfun),
        n'
        )

initValueToRegister n (UFunC i j body) r =
    let (comfun, n') = backend (s n) body
    in (
        [SetRegLabel r n, 
        SaveCtxToReg r],
        ((LABEL n) : 
         (AddEnv 2) :
         (SetEnvReg 1 1) :
         (SetEnvReg 0 2) :
        comfun),
        n'
        )

backend :: LABELNO -> Tailform -> ([MachL], LABELNO)
backend n (TCond ATrue b1 b2) = backend n b1 
backend n (TCond AFalse b1 b2) = backend n b2
backend n (TCond (NLVar i) b1 b2) =
    let (l1, n') = backend (s n) b1
    in let (l2, n'') = backend (s n') b2 
    in [IfEnvLabel i n n'] ++
        [LABEL n] ++ l1 ++ 
        [LABEL n'] ++ l2
        


backend n (TApp a1 a2) =
    let (running, funs, n') = mapValueToRegister [a1, a2] [0, 1] n
    in (running ++
        [Apply] ++
        funs, n') 

backend n (TAppc a1 a2 a3) =
    let (running, funs, n') = mapValueToRegister [a1, a2, a3] [0, 1, 2] n
    in (running ++
        [Apply] ++
        funs, n') 

backend n (EAdd a1 a2 a3) =
    let (running, funs, n') = mapValueToRegister [a1, a2, a3] [1, 2, 3] n
    in (running ++
        [ApplyInner ADD] ++
        funs, n') 
    
backend n (EMult a1 a2 a3) =
    let (running, funs, n') = mapValueToRegister [a1, a2, a3] [1, 2, 3] n
    in (running ++
    [ApplyInner MULT] ++
     funs, n') 

backend n (ENeg a1 a2) =
    let (running, funs, n') = mapValueToRegister [a1, a2] [1, 2] n
    in (running ++
        [ApplyInner NEG] ++
        funs, n') 

backend n (EInv a1 a2) =
    let (running, funs, n') = mapValueToRegister [a1, a2] [1, 2] n
    in (running ++
        [ApplyInner INV] ++
        funs, n') 

backend n (EPair a1 a2 a3) =
    let (running, funs, n') = mapValueToRegister [a1, a2, a3] [1, 2, 3] n
    in (running ++
        [ApplyInner PAIR] ++
        funs, n') 

backend n (ECar a1 a2) =
    let (running, funs, n') = mapValueToRegister [a1, a2] [1, 2] n
    in (running ++
        [ApplyInner CAR] ++
        funs, n') 

backend n (ECdr a1 a2) =
    let (running, funs, n') = mapValueToRegister [a1, a2] [1, 2] n
    in (running ++
        [ApplyInner CDR] ++
        funs, n') 

backend n (EZerop a1 a2) =
    let (running, funs, n') = mapValueToRegister [a1, a2] [1, 2] n
    in (running ++
        [ApplyInner ZEROP] ++
        funs, n')
        
backend n (ESys a1 a2 a3) =
    let (running, funs, n') = mapValueToRegister [(AConst a1), a2, a3] [1, 2, 3] n
    in (running ++
        [ApplyInner ZEROP] ++
        funs, n')

backend n (TLet i a body) =
    let (bodycompiled, n') = backend n body
    in 
    ([AddEnv,
    SetEnvEnv 0 i] ++ bodycompiled, n')

backend n (TLetRec fs ls body) =
    let numofrf = length fs
    in let (running, funs, n') = mapValueToRegister n (reverse ls) [1..numofrf]
    in let (bodycompiled, n'') = backend n' body
    in  (running ++
        [AddEnv numofrf] ++
        map (\(x, y)-> SetEnvReg x y) (zip [0 .. ] [1 .. numofrf]) ++
        map (\(x, y)-> SaveCtxToReg) [1 .. numofrf] ++        
        bodycompiled ++
        funs,
        n'')

        