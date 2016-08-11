module CPattern where
import Data.List (concat)


addheader :: String -> String -> String
addheader header = (("#include \"" ++ header ++ "\"\r") ++)


addcall :: String -> [String] -> String
addcall func (arg:args) = func ++ "(" ++ arg ++ (sepbyp args) ++ ")"
  where sepbyp [] = ""
        sepbyp args = concat . map (',':) $ args


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

sentence:: String -> String
sentence = (++ ";")

cube :: String -> String
cube x = ('{' : x) ++ "}" 

quotesentence ::String -> String
quotesentence = ('&':)
ifsentence :: String -> String -> String -> String
ifsentence pred branch1 branch2 =
  "if" ++ (addcall "" [pred]) ++ (cube branch1) ++ "else" ++ (cube branch2)

declfunc :: String -> String -> [String] -> String
declfunc funcname funcbody closurevar =
  (declcfunc (funcName funcname) funcbody) ++ 
  (declstruc funcname $ func' ++ closure') 
  where funcName = ("__FUNC__" ++)
        func' =
          constsentence .
          staticsentence .
          ptlongtype $ assignmentsentence "func" $ funcName funcname
        closure' =
          constsentence .
          staticsentence .
          ptlongtype $ pointerassignmentsentence "cls" $ constarray closurevar
declcfunc :: String -> String -> String
declcfunc funcname funcbody = "int " ++ (addcall funcname []) ++ (cube funcbody) 


declvar :: String -> String -> String
declvar name val = sentence $ ptlongtype $ name ++ "=" ++ val

declarray :: String -> Int -> String
declarray name i =sentence $ ptlongtype $ name ++ "[" ++ (show i) ++ "]"

declstruc :: String -> String -> String
declstruc name content = sentence $ "Struct " ++ (nameStruct name) ++ "{" ++ content ++ "}" ++ name
  where nameStruct = ("__STRUCT__" ++)

        
offsetof :: String -> Int -> String
offsetof array i = array ++ ("[" ++ show i ++ "]")

constarray :: [String] -> String
constarray (val:vals) = "[" ++ (foldl (\x y -> x ++ "," ++ y) val vals) ++ "]"
