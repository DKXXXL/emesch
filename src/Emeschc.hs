module Main where

    import qualified Parsing2 as P
    import Macro
    import AST
    import Frontend
    import IRep
    import Compiler
    import Backend
    import ToC

    import System.IO (stdin, stdout, hPutStr, hGetContents)

    stringtoAst :: String -> Exp 
    stringtoAst = toAst . macroTransformer . P.parser

    compile_fromAST :: Exp -> String
    compile_fromAST = genToC_ . toMachL_ . nameElimination_ . unifyVariableName_ . cpsOf_

    compile = compile_fromAST . stringtoAst

    main = do
        input <- hGetContents stdin 
        let output =  compile input
        hPutStr stdout output

    test = toMachL_ . nameElimination_ . unifyVariableName_ . cpsOf_ . stringtoAst $ "(let (i (lambda (x y) (+ x y))) (i 1 2))"
        