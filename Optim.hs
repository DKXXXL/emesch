import Register




--type Table = [[(String,Int)]] 
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
                delundef (LookVar _ x) xs = x:xs
                delundef (VarCatch _ x _) xs = x:xs
                delundef (DefVar x _) xs = filter (not . (==x)) xs
                delundef _ xs = xs
---Something wrong. The analyzation should be bottom-up and side-effect



lexAddr :: ICi -> ICi
lexAddr (ICi ops links b vars) =ICi  (fold'' lexAddr' ops [[],vars]) (withAll lexAddr links) b vars
  where lexAddr' :: ICop -> [[Cdata]] -> (ICop,[[Cdata]])
        lexAddr' (((SetVar x r)@org)) frames =
          case searchFrames frames x of Nothing -> ((org:(lexAddr' ops)),frames)
                                        (Just (a,b)) -> ((SetLVec (CInt a) (CInt b) r),frames)
        lexAddr' (((LookVar r x)@org)) frames =
          case searchFrames frames x of Nothing -> ((org:(lexAddr' ops)),frames)
                                        (Just (a,b)) -> ((GetLVec (CInt a) (CInt b) r),frames)

        lexAddr' (((VarCatch r x y)@org)) frames =
          case searchFrames frames x of Nothing ->
                                          ((org:(lexAddr' ops)),frames)
                                        (Just (a,b)) ->
                                          ((VarCatch' r (CInt a) (CInt b) x y),frames)
        lexAddr' (((DefVar x r)@org)) frames =
          (org, addinFrames frames x)
        lexAddr' (op) frames = (op,frames)
        find' :: [a] -> a -> Int
        find' (x:as) y = if x = y
                         then 0
                         else 1 + (find' as y)
        find' [] _ = 0
        searchFrames :: [[Cdata]] -> Cdata -> (Int,Int)
        searchFrames frames x =
          case foldl' sF frames (0,0) of ((length frames) + 1, 0) -> Nothing
                                         x -> Just x
          where sF :: (Int,Int) -> [Cdata] -> (Int,Int)
                sF (a,0) frame = (a + 1, find' frame x)
                sF (a,b) _ = (a,b)
        addinFrames (frame:frames) x = (x:frame):frames 
        fold'' :: (a -> b -> (a,b)) -> [a] -> b -> [a]
        fold'' f (a:l) b = let (nexta,nextb) = f a b
                           in (nexta : (fold'' f l nextb))
                
