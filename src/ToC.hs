import Text.Printf

toCLiteral :: Literal -> String
toCLiteral (LNumber n) = printf "SNUMBER(%s)" show n 
toCLiteral (LTrue) = "SBOOL(1)"
toCLiteral (LFalse) = "SBOOL(0)"
toCLiteral (LQuote s) = printf "SQUOTE(\"%s\")" s 
toCLiteral (LString s) = printf "SSTRING(\"%s\")" s

toC :: MachL -> String
toC (SetEnvReg a b) =
    printf "*(ENV(%d)) = reg%d;" a b

toC (SetEnvEnv a b) =
    printf "*(ENV(%d)) = *(ENV(%d));" a b

toC (SetRegEnv a b) =
    printf "reg%d = *(ENV(%d));" a b

toC (SetRegLabel a b) = 
    printf "reg%d = CLOSURE(LABEL%d);" a b

toC (SetRegLiteral a b) =
    printf "reg%d = %s;" a (toCLiteral b)

toC (LABEL x) =
    printf "}void LABEL%d(){" x 

toC Apply = "APPLY();"

toC (ApplyInner x) =
    show x ++ "();"
    
toC (SaveCtxToEnv offset) =
    printf "SAVCTX(*(ENV(%d)), env);" offset

toC (SaveCtxToReg r) =
    printf "SAVECTX(reg%d, env);" r

toC (AddEnv s) =
    printf "ADDENV(%d);" s

toC (IfEnvLabel e l1 l2) =
    printf "COND(*(ENV(%d)),GOTOLABEL(LABEL%d);,GOTOLABEL(LABEL%d););" e l1 l2

toCCode :: [MachL] -> String
toCCode =   (++ "}"). 
            foldl (\x y -> x ++ y) "void LABEL0{" . 
            (map toC)

regNum :: [MachL] -> Integer
regNum = maximum . map reNum'
        where regNum' :: MachL -> Integer
              regNum' (SetEnvReg _ x) = x 
              regNum' (SetRegEnv x _) = x 
              regNum' (SetRegLabel x _) = x 
              regNum' (SetRegLiteral x _) = x 
              regNum' (SaveCtxToReg x) = x
              regNum' _ = -1

toCDecl :: [MachL] -> String
toCDecl code = 
    "#include \"emeschlib.h\"" ++
    printf "VAR reg[%d];"  ((regNum code) + 1)
    
main :: String
main = "int main(){ENTRY();return 0;}"

genToC :: [MachL] -> String
genToC x = (toCDecl x) ++ (toCCode x)  ++ main
