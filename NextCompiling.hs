import Register
import Data.List (nub, last, init)
module NextCompiling(necessartTransform) where

{-
withAll :: (ICi -> ICi) -> (ICi -> ICi)
withAll f =
  \(ICi _ links _ _)@all -> let links' = map (\
-}
withAll :: (ICi -> ICi) -> [(Cdata,Cdata)] ->[(Cdata,Cdata)]
withAll f l = map (\(x,y) -> (x, case y of (CLambda y') -> f y'
                                           y' -> y')) l
  
foldstate :: ((Els a state) -> (Els [a] state)) -> [a] -> state -> (Els [a] state)
foldstate f (x:l) o = case f (x,o) of (x',o') -> case foldstate f l o' of (l',o'') -> (x'++l',o'')

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
----------------------These are necessary Transformation---------------------------------
necessaryTransform =callcTrs . lexTrs . callccTrs


callccTrs :: ICi -> ICi
callccTrs = callfunOpt . callccOptim

-----------------------------------------------------------------------------------------
---------CallccOpt :
newtype Els elem stat = (elem,stat)

callccOptim :: ICi -> ICi
callccOptim' ((ICi opss linkss c d e),oc) =
  let (ELs opss' (links',oc')) = foldstate callcco opss (linkss,oc)
  in let (Els links'' oc'')  =  foldstate callccolinks links' oc'
     in (ICi opss' links'' c d e, oc'')
  where  callccolinks :: (Els (Cdata,Cdata) Int) -> (Els [Cdata,Cdata] Int)
         callccolinks (Els (a,CLambda n) c) =
           let (i',c') = callccOptim' (n,c)
           in (Els [(a,CLambda $ i')] c')
         callccolinks (Els a b) = Els [a] b
         callcco :: (Els ICop ([(Cdata,Cdata)],c)) -> (Els [ICop] ([(Cdata,Cdata)],c))
         callcco (CCall r,(frame,c)) =
           let lamname = "TRBLAMBDA" ++ (show c)
               labname = "TRBLABEL" ++ (show c)
               back = CLambda $ ICi [Pop Argl Val,
                                     DefVar (CAtom "__val") Val,
                                     LookVar (CAtom "__argl") Val,
                                     Load Argl Val,
                                     LookVar (CAtom "__exp") Val,
                                     Load Exp Val,
                                     LookVar (CAtom "__ret") Val,
                                     Load Ret Val,
                                     LookVar (CAtom "__val") Val,
                                     Push Argl Val,
                                     LookVar (CAtom "__env") Val,
                                     Load Env Val,
                                     Pop Argl Val,
                                     Goto (CExItem labname)] []
                 [Val,Argl,Exp,Ret,Env] 
                 [(CAtom "__argl"),(CAtom "__exp"),(CAtom "__ret"),(CAtom "__env")]
                 []
           in ([Push Exp Val,
                Assign3 Val (CExItem lamname),
                Push Argl Val,
                Pop Exp Val,
                Call Val,
                Label (CExItem labname)],(((CExItem lamname),back):frame,c+1))
              
         callcco (Els x y) = Els [x] y
              

callfunOpt :: ICi -> ICi
callfunOpt (ICi ops links b c d) =
  ICi (foldr combine' [Callb] ops) (withall callfunOpt links) (b ++ [Ret,Env]) c d  
  where combine' :: ICop -> [ICop] -> [ICop]
        combine' (Call r) oops =[Callc r $ CLambda $ ICi oops [] [] []]
        combine' oop oops = oop : oops


callcTrs :: ICi -> ICi
callcTrs (ICi ops links b c d) =
  case last ops of (Callc r (Clambda l)@l') ->
                     ICi
                     ((init ops) ++ (Callc r (CExItem $ nameGenerator' l)))
                     (withAll callcTrs (CExItem $ nameGenerator' l ,l'):links)
                     b
                     c
                     d
                   x -> (ICi ops links b c d)

--------------------------------------------------------------------------------


lexTrs :: ICi -> ICi
lexTrs = lexAddr . lambdaVarMonoize . lambdaCatching

--------------------------------------------------------------------------------
----LexOpt :
lambdaCatching :: ICi -> ICi
lambdaCatching (ICi ops links using allvars vars) =
  let links' = withAll lambdaCatching links
  in varCatch . catchedVar $ ICi ops links' using vars
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
                               (\(_,x) -> case x of (CLambda (ICi _ _ _ vars)) -> vars) $
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
  ICi  (map (\x -> lexAddr' x [vars] [thisref]) ops) (withAll lexAddr links) b vars
  where lexAddr' :: ICop -> [[Cdata]] -> ICop
        lexAddr' (((SetVar x r)@org)) frames frames' =
          maybeIF
          (searchFrames frames x)
          org
          (\(a,b) ->
            maybeIF
            (searchFrames frames' x)
            (SetVar1 (CInt a) (CInt b) r)
            (\_ -> SetVar2 (CInt a) (CInt b) r))
          
        lexAddr' (((LookVar r x)@org)) frames frames' =
          maybeIF
          (searchFrames frames x)
          org
          (\(a,b) ->
            maybeIF
            (searchFrames frames' x)
            (GetVar1 (CInt a) (CInt b) r)
            (\_ -> GetVar2 (CInt a) (CInt b) r))


        lexAddr' (((VarCatch r x y)@org)) frames frames'=
          maybeIF
          (searchFrames frames x)
          org
          (\(a,b) ->
            maybeIF
            (searchFrames frames' x)
            (VarCatch1 r (CInt a) (CInt b) y)
            (\_ -> VarCatch2 r (CInt a) (CInt b) y))

        lexAddr' (((DefVar x r)@org)) frames frames'=
          case searchFrames frames x of Nothing -> ((org))
                                        (Just (a,b)) -> ((SetVar1 (CInt a) (CInt b) r))

        lexAddr' (op) frames = op
        maybeIF :: (Maybe a) -> b -> (a -> b) -> b
        maybeIF (Just x) _ f = f x
        maybeIF (Nothing) na _ = na
        searchFrames :: [[Cdata]] -> Cdata -> Maybe (Int,Int)
        searchFrames frames x =
          case foldl' sF frames (0,0) of ((length frames) + 1, 0) -> Nothing
                                         x -> Just x
          where sF :: (Int,Int) -> [Cdata] -> (Int,Int)
                sF (a,0) frame = (a + 1, find' frame x)
                sF (a,b) _ = (a,b)
                find' :: [a] -> a -> Int
                find' (x:as) y = if x = y
                                 then 0
                                 else 1 + (find' as y)
                find' [] _ = 0
 
