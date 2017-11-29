type T = L of string | N of T * string * T

let test = N (L "a", "b", N(L "c", "d", L "e"))
// T = Tree , N = Node
// contains string -> T -> bool
// Se illustration af træet i notesbog.

let rec contains s = function
    | L s'          -> s = s'
    | N(t1,s',t2) -> contains s t1 || s=s' || contains s t1 

// ["a","b",...,"e"]

// toList T -> string list
let rec toList = function
    | L s           -> [s]
    | N(t1,s',t2)   -> toList t1 @ [s'] @ toList t2


// replace : string -> T -> T -> T  L("a")

let rec replace s t = function
    | L s' when s = s'  -> t
    | N(t1,s',t2)       -> N(replace s t t1, s', replace s t t2)
    | t''               -> t''

replace "d" test

// naive version of river
type T' = N of int * T' list

// find max "flow rate"?

let rec maxT' (N(n,ts)) = List.fold (fun s t -> max s (t)) n ts

// different method

let rec maxT2' (N(n,ts)) = max n (maxT2L' ts)
and maxT2L' = function
                | [t] -> maxT2' t 
                | t::ts -> max(maxT2' t) (maxT2L' ts)

                