module Internal (Indata(..),Frames,lookupFrame,addtoFrame,delfromFrame,createNewFrame)
  where


data Indata =
  IQuote String
  | IBool Bool
  | IString String
  | INum Int
  | IProcedure ([Indata] -> (Indata,Frames))  
  | IList Indata Indata    
type Frame = [(String,Indata)]
type Frames = [Frame]

lookupFrames :: Frames -> String -> Indata

addtoFrames :: Frames -> (String,Indata) -> Frames

delfromFrames :: Frames -> String -> Frames

rewritetoFrames :: Frames -> (String, Indata) -> Frames

createNewFrame :: Frames -> [(String,Indata)] -> Frames

backFrames :: Frames -> Frames
