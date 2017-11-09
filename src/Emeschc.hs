module Main where

    import qualified Parsing2 as P
    import Macro
    import AST
    import Frontend
    import IRep
    import Compiler
    import Backend
    import ToC

    stringtoAst :: String -> Exp 
    stringtoAst = toAst . macroTransformer . P.parser

    compile_fromAST :: Exp -> String
    compile_fromAST = genToC_ . toMachL_ . nameElimination_ . unifyVariableName_ . cpsOf_

    compile = compile_fromAST . stringtoAst

    main = print (compile "(+ 1 1 1)")