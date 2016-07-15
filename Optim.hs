




type Table = [[(String,Int)]] 
{-
lexAddr :: ICi -> ICi
lexAddr = (\((ICi a b c),_,i) -> ICi a b ((LexVec i):c)) . (\x -> lexaddr (x, [[]], 0)) 
where lexaddr' :: (ICop,Table,Int) -> (ICop,Table,Int)
      lexaddr' (DefVar cd r, t, i) = (SetLVec (CInt i) r, addaddr' t (cd,i), i+1)
      lexaddr' (SetVar cd r, t, i) = (SetLVec (CInt $ lookaddr' t cd) r, t , i)
      lexaddr' (LookVar r cd, t, i) = (GetLVec (CInt $ lookaddr' t cd) r, t, i)
      lexaddr' (x, t, i) = (x, t, i)
      lexaddr (ICi ops links regs, t, i) =
        foldl lexaddr'acc  (proclinks links (ICi [] [] [], addframe' t ,i)) ops
        where lexaddr'acc (_, t, i) op = lexaddr' (op, t, i)
              lexaddracc (_, t, i) ici = lexaddr (ici, addframe' . backframe' $ t , i)
              proclinks links basic =
                foldl lexaddracc basic.
                map (\x -> case x of (_,CLabel y) -> y
                                     (_,_) -> ICI [] [] []) $ links
              addframe' :: Table -> Table
              addframe' = ([]:)
              backframe' :: Table -> Table
              backframe' (_:x) = x
              addaddr' (y:l) x = (x:y):l
              lookaddr' t cd =foldl maybeacc Nothing . map (find (\(x,_) -> x == cd)) $ t
                where maybeacc (Just x) _ = x
                      maybeacc _ (Just x) = x
                      --if maybeacc miss something, then error 
                     
-}
{-
withAll :: (ICi -> ICi) -> (ICi -> ICi)
withAll f =
  \(ICi _ links _ _)@all -> let links' = map (\
-}
withAll :: (ICi -> ICi) -> [(Cdata,Cdata)] ->[(Cdata,Cdata)]
withAll f l = map (\(x,y) -> (x, case y of (CLambda y') -> f y'
                                           y' -> y')) l
catchedVar :: ICi -> ICi
catchedVar (ICi ops a b vars) = ICi ops (withAll catchedVar a) b (catchedVar' ops)
  where catchedVar' =foldr delundef 
          where delundef :: ICop -> [Cdata] -> [Cdata]
                delundef (SetVar x _) xs = x:xs
                delundef (DefVar x _) xs = filter (not . (==x)) xs
                delundef _ xs = xs

lexAddr :: ICi -> ICi
lexAddr (ICi ops links b c) 

