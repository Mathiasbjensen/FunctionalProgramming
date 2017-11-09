type exp =
    | C of int
    | BinOp of exp * string * exp
    | Id of string
    | Def of string * exp * exp

// 2
let rec toString exp =
    match exp with
    | C a -> a.ToString()
    | BinOp (e1,s,e2) -> "("+toString e1 + s + toString e2+")"

toString (BinOp (C 2,"+", BinOp (C 5, "*", C 3)))

// 3
let rec extractOps exp =
    match exp with
    | C a -> Set.empty
    | BinOp(e1,s,e2) -> Set.union (Set.singleton s) (Set.union(extractOps e1)  (extractOps e2))

extractOps (BinOp (C 2,"+", BinOp (C 5, "*", C 3)))

// 4

let rec isDefAux exp s =
    match exp with
    | C _ -> true
    | Id s1 -> Set.contains s1 s
    | Def (s1,b,c) -> isDefAux b s && isDefAux c (Set.add s1 s)
    | BinOp (a,s1,c) -> isDefAux a s && isDefAux c s

let isDef exp = isDefAux exp Set.empty

isDef (Def ("x", C 5, BinOp (Id "x", "+", Id "x")))

isDef (Def ("x", C 5, BinOp (Id "y", "+", Id "x")))
