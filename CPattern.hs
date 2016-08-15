module CPattern where
import Data.List (concat, foldl')


addheader :: String -> String -> String
addheader header = (("#include \"" ++ header ++ "\"\r") ++)


addcall :: String -> [String] -> String
addcall func (arg:args) = func ++ "(" ++ arg ++ (sepbyp args) ++ ")"
  where sepbyp [] = ""
        sepbyp args = concat . map (',':) $ args
addcall func [] = func ++ "()"


assignmentsentence :: String -> String -> String
assignmentsentence to from = to ++ "=" ++ from ++ ";"

pointerassignmentsentence :: String -> String -> String
pointerassignmentsentence to from = assignmentsentence ("*" ++ to) from

staticsentence :: String -> String
staticsentence = (" static " ++)

constsentence :: String -> String
constsentence = (" const " ++)

ptlongtype :: String -> String
ptlongtype = (" ptlong " ++)

pointertype :: (String -> String) -> (String -> String)
pointertype f = ((f "*") ++)

sentence:: String -> String
sentence = (++ ";")

cube :: String -> String
cube x = ('{' : x) ++ "}" 

quotesentence ::String -> String
quotesentence = ('&':)

ifsentence :: String -> String -> String -> String
ifsentence pred branch1 branch2 =
  "if" ++ (addcall "" [pred]) ++ (cube branch1) ++ "else" ++ (cube branch2)

funcName = ("__FUNC__" ++)
instName = ("__INSTA__" ++)

declfunc :: String -> String -> [String] -> String
declfunc funcname funcbody closurevar =
  (declcfunc (funcName funcname) funcbody) ++ 
  (declstruc funcname $ func' ++ closure') 
  where func' =
          constsentence .
          staticsentence .
          ptlongtype $ assignmentsentence "func" $ funcName funcname
        closure' =
          strucvars closurevar
declcfunc :: String -> String -> String
declcfunc funcname funcbody = "void " ++ (addcall funcname []) ++ (cube funcbody) 


declvar :: String -> String -> String
declvar name val = sentence $ ptlongtype $ name ++ "=" ++ val



declarray :: String -> Int -> String
declarray name i =sentence $ staticsentence $ ptlongtype $ name ++ "[" ++ (show i) ++ "]"

declstruc :: String -> String -> String
declstruc name content =
  sentence $ "struct " ++ (nameStruct name) ++ "{" ++ content ++ "}" ++ (nameInstance name)
  where nameStruct = ("__STRUCT__" ++)
        nameInstance = ("__INSTA___" ++)

        
offsetof :: String -> Int -> String
offsetof array i = array ++ ("[" ++ show i ++ "]")

constarray :: [String] -> String
constarray (val:vals) = "[" ++ (foldl' (\x y -> x ++ "," ++ y) val vals) ++ "]"


strucvars :: [String] -> String
strucvars (var:vars) =
  foldl' (\x y -> x ++ (sentence . ptlongtype $ y)) (sentence.ptlongtype $ var) vars
strucvars [] = sentence ""
