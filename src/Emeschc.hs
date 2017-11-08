module Main where
    import Frontend
    import IRep
    import Compiler
    import Backend
    import ToC

    compile_fromAST :: Exp -> String
    compile_fromAST = genToC_ . toMachL_ . nameElimination_ . unifyVariableName_ . cpsOf_

    main = print (compile_fromAST (SConst 1))