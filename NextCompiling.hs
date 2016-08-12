module NextCompiling(necessaryTransform) where

import Register
import Data.List (foldl' ,nub, last, init, length)

{-
withAll :: (ICi -> ICi) -> (ICi -> ICi)
withAll f =
  \(ICi _ links _ _)@all -> let links' = map (\
-}
withAll :: (ICi -> ICi) -> [(Cdata,Cdata)] ->[(Cdata,Cdata)]
withAll f l = map (\(x,y) -> (x, case y of (CLambda y') -> CLambda $ f y'
                                           y' -> y')) l
  
foldstate :: ((Els a state) -> (Els [a] state)) -> [a] -> state -> (Els [a] state)
foldstate f (x:l) o = case f (Els x o) of (Els x' o') ->
                                            case foldstate f l o' of (Els l' o'') ->
                                                                       (Els (x'++l') o'')

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
----------------------These are necessary Transformation---------------------------------
necessaryTransform = delDup . callcTrs . lexTrs . callccTrs


callccTrs :: ICi -> ICi
callccTrs = callfunOpt . callccOptim

-----------------------------------------------------------------------------------------
---------CallccOpt :
data Els elem stat = Els elem stat

callccOptim :: ICi -> ICi
callccOptim x = (\(Els x _) -> x) $ callccOptim' (Els x 0)
callccOptim' (Els (ICi opss linkss c d e) oc) =
  let (Els opss' (links',oc')) = foldstate callcco opss (linkss,oc)
  in let (Els links'' oc'')  =  foldstate callccolinks links' oc'
     in (Els (ICi opss' links'' c d e) oc'')
  where  callccolinks :: (Els (Cdata,Cdata) Int) -> (Els [(Cdata,Cdata)] Int)
         callccolinks (Els (a,CLambda n) c) =
           let (Els i' c')  = callccOptim' $ Els n c
           in (Els [(a,CLambda $ i')] c')
         callccolinks (Els a b) = Els [a] b
         callcco :: (Els ICop ([(Cdata,Cdata)], Int)) -> (Els [ICop] ([(Cdata,Cdata)], Int))
         callcco (Els (CCall r) (frame,c)) =
           let lamname = "TRBLAMBDA" ++ (show c)
               labname = "TRBLABEL" ++ (show c)
               back = CLambda $ ICi [Pop Argl Val,
                                     DefVar (CAtom "__val") Val,
                                     LookVar Val (CAtom "__argl"),
                                     Load Argl Val,
                                     LookVar Val (CAtom "__exp"),
                                     Load Exp Val,
                                     LookVar Val (CAtom "__ret"),
                                     Load Ret Val,
                                     LookVar Val (CAtom "__val"),
                                     Push Argl Val,
                                     LookVar Val (CAtom "__env"),
                                     Load Env Val,
                                     Pop Argl Val,
                                     Goto (CExItem labname)] []
                 [Val,Argl,Exp,Ret,Env] 
                 [(CAtom "__argl"),(CAtom "__exp"),(CAtom "__ret"),(CAtom "__env")]
                 []
           in (Els [Push Exp Val,
                    Assign3 Val (CExItem lamname),
                    Push Argl Val,
                    Pop Exp Val,
                    Call Val,
                    Label (CExItem labname)] (((CExItem lamname),back):frame, c+1))
              
         callcco (Els x y) = Els [x] y
              

callfunOpt :: ICi -> ICi
callfunOpt (ICi ops links b c d) =
  ICi (foldr combine' [Callb] ops) (withAll callfunOpt links) (b ++ [Ret,Env]) c d  
  where combine' :: ICop -> [ICop] -> [ICop]
        combine' (Call r) oops =[Callc r $ CLambda $ ICi oops [] [] [] []]
        combine' oop oops = oop : oops


callcTrs :: ICi -> ICi
callcTrs (ICi ops links b c d) =
  case last ops of (Callc r l'@(CLambda l)) ->
                     ICi
                     ((init ops) ++ [Callc r (CExItem $ nameGenerator' l)])
                     (withAll callcTrs ((CExItem $ nameGenerator' l ,l'):links))
                     b
                     c
                     d
                   x -> (ICi ops links b c d)

--------------------------------------------------------------------------------


lexTrs :: ICi -> ICi
lexTrs = lexAddr . lambdaCatching . lambdaVarMonoize

--------------------------------------------------------------------------------
----LexOpt :
lambdaCatching :: ICi -> ICi
lambdaCatching (ICi ops links using allvars vars) =
  let links' = withAll lambdaCatching links
  in varCatch . catchedVar $ ICi ops links' using allvars vars
     where catchedVar :: ICi -> ICi
           catchedVar (ICi ops a b c vars) = ICi ops a b c (catchedVar' ops)
             where catchedVar' = foldr delundef [] 
                     where delundef :: ICop -> [Cdata] -> [Cdata]
                           delundef (SetVar x _) xs = x:xs
                           delundef (LookVar _ x) xs = x:xs
                           delundef (VarCatch _ x _) xs = x:xs
                           delundef (DefVar x _) xs = filter (not . (==x)) xs
                           delundef _ xs = xs

           varCatch :: ICi -> ICi
           varCatch (ICi ops links b c d) =  ICi (concat $ map varCatch' ops) links b c d
             where varCatch' :: ICop -> [ICop]
                   varCatch' (Assign3 r l) = varcatchLambda
                     where varcatchLambda =
                             ((Assign2 r l):
                              (varCatchLambda $
                               (\(Just (_,x)) -> case x of (CLambda (ICi _ _ _ _ vars)) -> vars) $
                               find'' links (\(x,y) -> x == l)))
                             where varCatchLambda :: [Cdata] -> [ICop]
                                   varCatchLambda = map (\x -> VarCatch Val x l)
                   varCatch' x = [x]
                   find'' :: [a] -> (a -> Bool) -> Maybe a
                   find'' (x:y) f = if f x
                                    then Just x
                                    else find'' y f
                   find'' [] _ = Nothing

lambdaVarMonoize :: ICi -> ICi
lambdaVarMonoize (ICi a links r vars ref) =
  ICi a (withAll lambdaVarMonoize links) r (monoize vars) ref
  where monoize = nub

lexAddr :: ICi -> ICi
lexAddr (ICi ops links b vars thisref) =
  ICi  (map (\x -> lexAddr' x [vars] [thisref]) ops) (withAll lexAddr links) b vars thisref
  where lexAddr' :: ICop -> [[Cdata]] -> [[Cdata]] -> ICop
        lexAddr' ((org@(SetVar x r))) frames frames' =
          maybeIF
          (searchFrames frames x)
          org
          (\(a,b) ->
            maybeIF
            (searchFrames frames' x)
            (SetVar1 (CInt a) (CInt b) r)
            (\_ -> SetVar2 (CInt a) (CInt b) r))
          
        lexAddr' ((org@(LookVar r x))) frames frames' =
          maybeIF
          (searchFrames frames x)
          org
          (\(a,b) ->
            maybeIF
            (searchFrames frames' x)
            (GetVar1 (CInt a) (CInt b) r)
            (\_ -> GetVar2 (CInt a) (CInt b) r))


        lexAddr' ((org@(VarCatch r x y))) frames frames'=
          maybeIF
          (searchFrames frames x)
          org
          (\(a,b) ->
            maybeIF
            (searchFrames frames' x)
            (VarCatch1 r (CInt a) (CInt b) y)
            (\_ -> VarCatch2 r (CInt a) (CInt b) y))

        lexAddr' ((org@(DefVar x r))) frames frames'=
          case searchFrames frames x of Nothing -> ((org))
                                        (Just (a,b)) -> ((SetVar1 (CInt a) (CInt b) r))

        lexAddr' (op) _ _= op
        maybeIF :: (Maybe a) -> b -> (a -> b) -> b
        maybeIF (Just x) _ f = f x
        maybeIF (Nothing) na _ = na
        searchFrames :: [[Cdata]] -> Cdata -> Maybe (Int,Int)
        searchFrames frames x =
          let k = (foldl' sF (0,0) frames) 
          in if k == ((length frames) + 1, 0)
             then Nothing
             else Just k
          where sF :: (Int,Int) -> [Cdata] -> (Int,Int)
                sF (a,0) frame = (a + 1, find' frame x)
                sF (a,b) _ = (a,b)
                find' ::(Eq a) => [a] -> a -> Int
                find' (x:as) y = if x == y
                                 then 0
                                 else 1 + (find' as y)
                find' [] _ = 0
 
-----------------------
--VarName transform--

iflegal :: Cdata -> Bool
iflegal = \_ -> True

legalfy :: Cdata -> Cdata
legalfy = \x -> x

transform :: Cdata -> Cdata
transform (CExItem l) = CExItem $ transform' l
  where transform' :: String -> String
        transform' (x:y) = y ++ [x,x]

delDup :: ICi -> ICi
delDup x = case delDup'' $ Els x [CExItem "main"] of Els ret _ -> ret 
  where delDup' :: Els ICi [Cdata] -> Els ICi [Cdata]
        delDup' (Els i@(ICi a links b c d) table) =
          let ifcover' :: [Cdata] -> [Cdata] -> [Cdata]
              ifcover' x y = filter (\x -> (not $ iflegal x) ||
                                           (foldl' (||) False $ map (\y -> (x == y)) y)) x
              covertable = ifcover' (map fst links) table
          in let legalcover :: [Cdata] -> [Cdata]
                 legalcover org = let org' = ifcover' org table
                                  in if org' == []
                                     then org
                                     else legalcover (map (legalfy . transform) org)
             in let newnames = legalcover covertable
                in Els (changename i (zip covertable newnames)) (table ++ newnames)
          where changename :: ICi -> [(Cdata,Cdata)] -> ICi
                changename (ICi ops links rs vars refs) table =
                  ICi
                  (map changenameop ops)
                  (map changenamelink links)
                  rs
                  (map changenamevar vars)
                  (map changenamevar refs)
                  where transform''' :: Cdata -> Cdata
                        transform''' v = snd . head $ filter (\(x,_) -> x == v) table 
                        changenameop :: ICop -> ICop
                        changenameop (Assign2 r v) = Assign2 r (transform''' v)
                        changenameop (Assign3 r v) = Assign3 r (transform''' v)
                        changenameop (Label v) = Label (transform''' v)
                        changenameop (Goto v)  = Goto (transform''' v)
                        changenameop (Callc r v) = Callc r (transform''' v)
                        changenameop (VarCatch r v1 v2) =
                          VarCatch r (transform''' v1) (transform''' v2)
                        changenameop (VarCatch1 r v1 v2 v3) =
                          VarCatch1 r (transform''' v1) (transform''' v2) (transform''' v3)
                        changenameop (VarCatch2 r v1 v2 v3) =
                          VarCatch2 r (transform''' v1) (transform''' v2) (transform''' v3)
                        changenameop (LookVar r v) = LookVar r (transform''' v)
                        changenameop (SetVar v1 r) =
                          SetVar  (transform''' v1)  r
                        changenameop (SetVar1 v1 v r) =
                          SetVar1  (transform''' v1) (transform''' v) r
                        changenameop (GetVar v1 r) =
                          GetVar  (transform''' v1)  r
                        changenameop (SetVar2 v1 v r) =
                          SetVar2  (transform''' v1) (transform''' v) r
                        changenameop (GetVar1 v1 v r) =
                          GetVar1  (transform''' v1) (transform''' v) r
                        changenameop (GetVar2 v1 v r) =
                          GetVar2  (transform''' v1) (transform''' v) r
                        changenameop (DefVar v r) = DefVar  (transform''' v) r
                        changenamelink (v1,v2) = (transform''' v1, v2)
                        changenamevar v1 = transform''' v1
        delDup'' :: Els ICi [Cdata] -> Els ICi [Cdata]
        delDup'' (Els  (all@(ICi a links b c d)) state) =
          let Els links' state' =
                foldl'
                (\(Els l table) (y1,CLambda y2) ->
                  case delDup'' (Els y2 table) of (Els y2' table') ->
                                                    Els ((y1,CLambda y2'):l) table')
                (Els [] state) links 
          in delDup' $ Els (ICi a links' b c d) state'
             


{-
treeToline :: [(Cdata,Cdata)] -> [(Cdata,Cdata)]
treeToline x =
  (treeToline .
   concat $
   map (\(_,y) -> case y of (CLambda $ ICi _ linkages' _ _ _) -> linkages'
                            y' -> []) x) ++
  (map (\(k,y) -> case y of (CLambda $ ICi a _ b c d) -> (k, (CLambda $ ICi a [] b c d))
                            y' -> (k,y)) x)
  -- Width Prior Explore
  
delDup' :: [(Cdata,Cdata)] -> [(Cdata,Cdata)]
delDup' = nubBy (\(a,b) (c,d) -> (a == c) && (b == d))
delDup'' :: [(Cdata,Cdata)] -> [(Cdata,Cdata)]
delDup''' :: Els [(Cdata,Cdata)] Int-> [(Cdata,Cdata)]

-}
