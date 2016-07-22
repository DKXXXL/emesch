import Register



{-
withAll :: (ICi -> ICi) -> (ICi -> ICi)
withAll f =
  \(ICi _ links _ _)@all -> let links' = map (\
-}
withAll :: (ICi -> ICi) -> [(Cdata,Cdata)] ->[(Cdata,Cdata)]
withAll f l = map (\(x,y) -> (x, case y of (CLambda y') -> f y'
                                           y' -> y')) l

lambdaCatching :: ICi -> ICi
lambdaCatching (ICi ops links using vars) =
  let links' = withAll (varCatch . catchedVar . lambdaCatching) links
  in varCatch . catchedVar $ ICi ops links' using vars
     where catchedVar :: ICi -> ICi
           catchedVar (ICi ops a b vars) = ICi ops a b (catchedVar' ops)
             where catchedVar' =foldr delundef 
                     where delundef :: ICop -> [Cdata] -> [Cdata]
                           delundef (SetVar x _) xs = x:xs
                           delundef (LookVar _ x) xs = x:xs
                           delundef (VarCatch _ x _) xs = x:xs
                           delundef (DefVar x _) xs = filter (not . (==x)) xs
                           delundef _ xs = xs

           varCatch :: ICi -> ICi
           varCatch (ICi ops links b c) =
             where varCatch' :: [ICop] -> [ICop]
                   varCatch' ((Assign3 r l):xs) = varcatchLambda ++ (varCatch' xs)
                     where varcatchLambda =
                             ((Assign2 r l):
                              (varCatchLambda $
                               (\(_,x) -> case x of (CLambda (ICi _ _ _ vars)) -> vars) $
                               find'' links (\(x,y) -> x == l)))
                             where varCatchLambda :: [Cdata] -> [ICop]
                                   varCatchLambda = map (\x -> VarCatch Val x l)
                   varCatch' (x:xs) = x : (varCatch' xs)
                   find'' :: [a] -> (a -> Bool) -> Maybe a
                   find'' (x:y) f = if f x
                                    then Just x
                                    else find'' y f
                   find'' [] _ = Nothing


lexAddr :: ICi -> ICi
lexAddr (ICi ops links b vars) =ICi  (fold'' lexAddr' ops [vars]) (withAll lexAddr links) b vars
  where lexAddr' :: ICop -> [[Cdata]] -> (ICop,[[Cdata]])
        lexAddr' (((SetVar x r)@org)) frames =
          case searchFrames frames x of Nothing -> ((org:(lexAddr' ops)),frames)
                                        (Just (a,b)) -> ((SetVar' (CInt a) (CInt b) r),frames)
        lexAddr' (((LookVar r x)@org)) frames =
          case searchFrames frames x of Nothing -> ((org:(lexAddr' ops)),frames)
                                        (Just (a,b)) -> ((GetVar' (CInt a) (CInt b) r),frames)

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
                
foldstate :: ((a,state) -> ([a],state)) -> [a] -> state -> ([a],state)
foldstate f (x:l) o = case f (x,o) of (x',o') -> case foldstate f l o' of (l',o'') -> (x'++l',o'')
{-      
callccOptim :: ICi -> ICi
callccOptim' ((ICi opss linkss c d),oc) =
  let (opss',(links',oc')) = foldstate callcco opss (linkss,oc)
  in let (links'',oc'')  =  foldstate callccolinks links' oc'
     in (ICi opss' links'' c d, oc'')
  where  callccolinks :: ((Cdata,Cdata),Int) -> ([Cdata,Cdata],Int)
         callccolinks ((a,CLambda n),c) =
           let (i',c') = callccOptim' (n,c)
           in ([(a,CLambda $ i')],c')
         
         callcco :: (ICop, ([(Cdata,Cdata)],c)) -> ([ICop],([(Cdata,Cdata)],c))
         callcco (CCall r,(frame,c)) =
           let lamname = "TRBLAMBDA" ++ (show c)
               labname = "TRBLABEL" ++ (show c)
               back = CLambda $ ICi [Pop Argl Val,
                                     DefVar (CAtom "__ret") Val,
                                     LookVar (CAtom "__argl") Val,
                                     Load Argl Val,
                                     LookVar (CAtom "__exp") Val,
                                     Load Exp Val,
                                     LookVar (CAtom "__ret") Val,
                                     Goto (CExItem labname)] [] [Val,Argl,Exp] []
         
           in ([Push Exp Val,
                Assign3 Val (CExItem lamname),
                Push Argl Val,
                Pop Exp Val,
                Call Val,
                Label (CExItem labname)],(((CExItem lamname),back):frame,c+1))
              
         callcco (x,y) = ([x],y)
              
-}
