type FileSys = Element list 
    and Element =
    | File of string * string
    | Dir of string * FileSys

// ListOfNames
let d1 = 
    Dir("d1",[File("a1","java"); 
        Dir("d2", [File("a2","fsx"); 
            Dir("d3", [File("a3","fs")])]); 
        File("a4","fsx"); 
        Dir("d3", [File("a5","pdf")])]);;


let rec namesFileSys = function 
    | [] -> [] 
    | e::es -> (namesElement e) @ (namesFileSys es) 
and namesElement = function 
    | File (s,s1) -> [s+"."+s1] 
    | Dir(s,fs) -> s :: (namesFileSys fs)

namesElement d1

// search

let rec searchFileSys ext = function
    | [] -> Set.empty
    | e::es -> Set.union (searchElement ext e) (searchFileSys ext es)
and searchElement ext = function
    | File (s,s1) -> if s1 = ext then Set.singleton s else Set.empty
    | Dir (s,fs) -> searchFileSys ext fs

searchElement "fsx" d1


// longNames

let rec longNamesFileSys = function
    | [] -> Set.empty
    | e::es -> Set.union (longNamesElement e) (longNamesFileSys es) 
and longNamesElement = function
    | File (s,s1) ->  set [s+"."+s1]
    | Dir (s,fs) -> Set.map ((+) (s+"\\")) (longNamesFileSys fs)

longNamesElement d1

// 6.2
type Fexpr = | Const of float
             | X
             | Add of Fexpr * Fexpr
             | Sub of Fexpr * Fexpr
             | Mul of Fexpr * Fexpr
             | Div of Fexpr * Fexpr
             | Sin of Fexpr
             | Cos of Fexpr
             | Log of Fexpr
             | Exp of Fexpr;;

let rec toString = function
    | Const f       -> string f
    | X             -> "x"
    | Add (f1,f2)   -> "(" + toString f1 + "+" + toString f2 + ")"
    | Sub (f1,f2)   -> "(" + toString f1 + "-" +  toString f2 + ")"
    | Mul (f1,f2)   -> "(" + toString f1 + "*" + toString f2 + ")"
    | Div (f1,f2)   -> "(" + toString f1 + "/" + toString f2 + ")"
    | Sin f         -> "sin(" + toString f +  ")"
    | Cos f         -> "cos(" + toString f + ")"
    | Log f         -> "log(" + toString f +  ")"
    | Exp f         -> "exp(" + toString f +  ")"

toString (Sin (Mul(X, X)))

// 6.8
type Instruction = | ADD |  SUB | MULT | DIV | SIN 
                   | COS | LOG | EXP | PUSH of float
               
type Stack = float list

let rec intpInstr = function
    | (_,[])                -> []
    | (ADD,Stack x::xs::rst)      -> x+xs::rst
    | (SUB,x::xs::rst)      -> x-xs::rst
    | (MULT,x::xs::rst)     -> x*xs::rst
    | (DIV,x::xs::rst)      -> x/xs::rst
    | (SIN,x::xs)           -> sin(x)::xs
    | (COS,x::xs)           -> cos(x)::xs
    | (LOG,x::xs)           -> log(x)::xs
    | (EXP,x::xs)           -> exp(x)::xs
    | (PUSH a,x)            -> a::x
