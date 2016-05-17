import Data.List (concat)

addheader :: String -> String -> String
addheader header = "#include \"" ++ header ++ "\"\r"


addcall :: String -> [String] -> String
addcall func (arg:args) = func ++ "(" ++ arg ++ (sepbyp args) ++ ")"
  where sepbyp [] = ""
        sepbyp args = concat . map (',':) $ args


assignmentsentence :: String -> String -> String
assignmentsentence to from = to ++ "=" ++ from ++ ";"

sentence:: String -> String
sentence = (++ ";")

cube :: String -> String
cube x = ('{' : x) ++ "}" 

quotesentence ::String -> String
quotesentence = '&':
ifsentence :: String -> String -> String -> String
ifsentence pred branch1 branch2 =
  "if" ++ (addcall "" [pred]) ++ (branch1) ++ "else" ++ (branch2)

declfunc :: String -> String -> String
declfunc funcname funcbody = "int " ++ (addcall funcname []) ++ (cube funcbody) 

declvar :: String -> String -> String
declvar name val = "ptlong " ++ name ++ "=" ++ val ++ ";"

declarray :: String -> Int -> String
declarray name i = "ptlong " ++ name ++ "[" ++ (show i) ++ "];"

offsetof :: String -> Int -> String
offsetof array i = array ++ ("[" ++ show i ++ "]")
