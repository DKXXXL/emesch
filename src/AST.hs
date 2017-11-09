module AST where
    import qualified Parsing2 as P
    import Macro
    import Frontend
    import Data.Char (ord)


    rename :: [Char] -> Integer
    rename (x:[]) = toInteger . ord $ x
    rename (x:s) = (toInteger . ord $ x) + (rename s) * 128


    toAst :: P.SStruc -> Exp
    toAst (P.SString s) = SString s
    toAst (P.SBool t) = if t then STrue else SFalse
    toAst (P.SNum i) = SConst (toInteger i)
    toAst (P.SQuote (P.SAtom s)) = SQuote s 
    toAst (P.SAtom x) = SVar (rename x)
    toAst (P.SList ((P.SAtom "quote"):(P.SAtom s):[])) =
        SQuote s
    toAst (P.SList ((P.SAtom "lambda"):(P.SList ((P.SAtom x):[])):body:[])) =
        SFun (rename x) (toAst body)
    toAst (P.SList ((P.SAtom "let"):(P.SList ((P.SAtom x):bind:[])):body:[])) =
        SLet (rename x) (toAst bind) (toAst body)
    toAst (P.SList ((P.SAtom "letrec"):(P.SList binds):body:[])) =
        let (fs, xs, binds') = eachtoAst binds
        in SLetRec fs xs binds' (toAst body)
        where eachtoAst :: [P.SStruc] -> ([Symbol], [Symbol],[Exp])
              eachtoAst = group' .(map eachtoAst' )
                where group' :: [(a,b,c)] -> ([a], [b], [c])
                      group' ((x,y,z):ls) =
                            let (xs,ys,zs) = group' ls
                            in (x:xs, y:ys, z:zs)
                      group' [] = ([],[],[]) 
                      eachtoAst' :: P.SStruc -> (Symbol, Symbol, Exp)
                      eachtoAst' (P.SList ((P.SList ((P.SAtom f):(P.SAtom x):[])):bindf:[])) =
                          (rename f, rename x, toAst bindf)
            


    toAst (P.SList (dec:oprands)) =
        let operands' = map toAst oprands
        in if(carefulOperator dec)
            then case dec of (P.SAtom "+") -> case operands' of a:b:[] -> SAdd a b
                             (P.SAtom "*") -> case operands' of a:b:[] -> SMult a b
                             (P.SAtom "zerop") -> case operands' of a:[] -> SZerop a
                             (P.SAtom "if") -> case operands' of a:b:c:[] -> SCond a b c
                             (P.SAtom "cons") -> case operands' of a:b:[] -> SPair a b 
                             (P.SAtom "car") -> case operands' of a:[] -> SCar a 
                             (P.SAtom "cdr") -> case operands' of a:[] -> SCdr a 
                             (P.SAtom "neg") -> case operands' of a:[] -> SNeg a 
                             (P.SAtom "inv") -> case operands' of a:[] -> SInv a 
                             (P.SAtom "begin") -> SBegin operands'
                             (P.SAtom "sys") -> case operands' of (SConst i):b:[] -> SSys i b
            else case operands' of a:[] -> SApp (toAst dec) a
                            
                            