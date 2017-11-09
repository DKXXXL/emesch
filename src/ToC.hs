module ToC where
    import IRep
    import Text.Printf

    toCLiteral :: Literal -> String
    toCLiteral (LNumber n) = printf "SNUMBER(%s)" $ show n 
    toCLiteral (LTrue) = "SBOOL(1)"
    toCLiteral (LFalse) = "SBOOL(0)"
    toCLiteral (LQuote s) = printf "SQUOTE(\"%s\")" s 
    toCLiteral (LString s) = printf "SSTRING(\"%s\")" s

    toC :: MachL -> String
    toC (SetEnvReg a b) =
        printf "*(ENV(%d)) = reg[%d];" a b

    toC (SetEnvEnv a b) =
        printf "*(ENV(%d)) = *(ENV(%d));" a b

    toC (SetRegEnv a b) =
        printf "reg[%d] = *(ENV(%d));" a b

    toC (SetRegLabel a b) = 
        printf "reg[%d] = CLOSURE(LABEL%d);" a b

    toC (SetRegLiteral a b) =
        printf "reg[%d] = %s;" a (toCLiteral b)

    toC (LABEL x) =
        printf "}void LABEL%d(){" x 

    toC Apply = "APPLY();"

    toC (ApplyInner x) =
        show x ++ "();"
        
    toC (SaveCtxToEnv offset) =
        printf "SAVCTX(*(ENV(%d)), env);" offset

    toC (SaveCtxToReg r) =
        printf "SAVECTX(reg[%d], env);" r

    toC (AddEnv s) =
        printf "ADDENV(%d);" s

    toC (IfEnvLabel e l1 l2) =
        printf "COND(*(ENV(%d)),JUMPLABEL(LABEL%d);,JUMPLABEL(LABEL%d););" e l1 l2

    toCCode :: [MachL] -> String
    toCCode =   (++ "}"). 
                foldl (\x y -> x ++ "\n" ++ y) "void LABEL0(){" . 
                (map toC)

    regNum :: [MachL] -> Integer
    regNum = maximum . map regNum'
            where regNum' :: MachL -> Integer
                  regNum' (SetEnvReg _ x) = x 
                  regNum' (SetRegEnv x _) = x 
                  regNum' (SetRegLabel x _) = x 
                  regNum' (SetRegLiteral x _) = x 
                  regNum' (SaveCtxToReg x) = x
                  regNum' _ = -1


    labelNum :: [MachL] -> Integer
    labelNum = maximum . map labelNum'
        where labelNum' :: MachL -> Integer
              labelNum' (LABEL i) = i
              labelNum' _ = -1
        
    toCDecl :: [MachL] -> String
    toCDecl code = 
        let rn = (regNum code)
        in "#include \"emeschlib.h\" \n" ++
            (printf "static const int REGNUM = %d; \n"  (rn + 1)) ++
            (printf "VAR reg[%d];"  (rn + 1)) ++
            (foldl1 (\x y -> x ++ y) $ map (\x -> printf "void LABEL%d();" x) [0..(labelNum code)])
        
    mainC :: String
    mainC = "int main(){ENTRY();return 0;}"

    genToC_ :: [MachL] -> String
    genToC_ x = (toCDecl x) ++ (toCCode x)  ++ mainC
