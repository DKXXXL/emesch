module Main where
import Emesch (compiles', compiles, CompileArg(..))

import System.Environment (getArgs)
import System.IO (stdin, stdout, hPutStr, hGetContents)

argHandle :: (String, [String]) -> CompileArg
argHandle ("-O", x) = Opt x

-- argHandle ("-o", x) = Output  $ concat x
-- argHandle ("-c", x) = Target x
argHandle (_ , x) = Failure

argsTovargs :: [String] -> [(String,[String])]
argsTovargs x = argsTovargs' x [] 
  where argsTovargs' :: [String] -> [(String,[String])] -> [(String,[String])]
        argsTovargs' (x:l) (rs'@((rx,ry):rs)) = case x of ('-':x') -> argsTovargs' l ((x',[]):rs')
                                                          x' -> argsTovargs' l ((rx, x':ry):rs)
        argsTovargs' (x:l) [] = case x of ('-': x') -> argsTovargs' l ((x',[]):[])
                                          x' -> argsTovargs' l (("-c", [x']):[])
        argsTovargs' [] a = a

main :: IO ()
main = do
  args <- getArgs
  input <- hGetContents stdin 
  let output =  compiles'  ((map argHandle) . argsTovargs $ args) input
  hPutStr stdout output
