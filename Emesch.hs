module Emesch(compiles', compiles, CompileArg(..)) where

import Register
import Parsing2 (parser)
import Compiling (allcompile)
import Opts(opts, optSI)

data CompileArg =
  Opt [String]
  | Target [String]
  | Failure
  | Output String

compiles' :: [CompileArg] -> String
compiles' = concat . compiles

compiles :: [CompileArg] -> [String]
compiles args =
  map (allcompile (getopt args)  . parser) $
  gettarget args 
  where getopt :: [CompileArg] -> ICi -> ICi 
        getopt = opts . optSI. concat . map (\x -> case x of (Opt x') -> x'
                                                             (x') -> []) 
        
        gettarget :: [CompileArg] -> [String]
        gettarget = concat . map (\x -> case x of (Target x') -> x'
                                                  x' -> [])
