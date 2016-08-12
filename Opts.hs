module Opts(opts, optSI) where

import Register
import Data.List


optSI :: [String] -> [Int]
optSI x = []
opts :: [Int] -> ICi -> ICi
opts =
  (foldl' (\x y -> x . y) id') . 
  (map opts') 
  where opts' :: Int -> ICi -> ICi
        opts' x = id'
        id' x = x




  
