module Emesch(
  compiles
  , compilestest'
  , compiles'
  ,CompileArg(..)
  ) where

import Register
import Parsing2
import Compiling 
import Opts(opts, optSI)

data CompileArg =
  Opt [String]
  | Target [String]
  | Failure
  | Output String
compiles' :: [CompileArg] -> String -> String
compiles' x y = concat $ compiles x [y]


--compilestest' :: [CompileArg] -> String -> ICi
compilestest' x y = compiletest (\x -> x) . parser $ y

{-
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
-}

compiles :: [CompileArg] -> [String] -> [String]
compiles args filestreams =
  map (allcompile (getopt args)  . parser) $
  filestreams
  where getopt :: [CompileArg] -> ICi -> ICi 
        getopt = opts . optSI. concat . map (\x -> case x of (Opt x') -> x'
                                                             (x') -> []) 
        
--        gettarget :: [CompileArg] -> [String]
--        gettarget = concat . map (\x -> case x of (Target x') -> x'


