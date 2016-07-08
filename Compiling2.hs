



compile :: SStruc -> ICi

compile (SString x) =ICi [Assign2 Val (CString x)][Val] []
compile (SNum x) =ICi [Assign2 Val (CInt x)] [Val] []
compile (SBool x) =ICi [Assign2 Val (CBool x)] [Val] []
compile (SAtom x) =ICi [LookVar Val (CAtom x)] [Val] [CAtom x]

compile (SList ((SAtom "define"):(SList ((SAtom funcName):args)):body:[])) =
  compile (SList ((SAtom "define"):(SAtom funcName):(SList (SAtom "lambda"):(SList args):body)))


compile (SList (func:args)) =
  acobC ((compile func):
         (ICi [Push Exp Val] [Exp,Val] []):
         (compileArgs args):[])
  where compileArgs :: [SStruc] -> ICi
        compileArgs =
          acobC . concat $
          map (\x -> ((compile x):
                      (ICi [Push Argl Val] [Argl,Val] []):[]))
  

acobC :: [ICi] -> ICi
acobC = ICi
  (concat . map . ops $ icis)
  (nub . concat . map . using $ icis) 
  (nub . concat . map . var $ icis) 
