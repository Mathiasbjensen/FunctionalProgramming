// May 2017 

// Problem 2
// 1

let rec f = function
    | 0 -> [0]
    | i when i>0 -> i::g(i-1)
    | _ -> failwith "Negative argument"

and g = function
    | 0 -> []
    | n -> f(n-1);;

let h s k = seq { for a in s do
    yield k a };;


f 5 // = [5;3;1]


h (seq [1;2;3;4]) (fun i -> i+10) // = [11;12;13;14]

let rec sum xs = 
    match xs with
    | [] -> 0
    | x::rest -> x + sum rest;;

// f: int -> int list
// h: seq<a> -> k(a->b) -> seq<b>

// 2 - Make sum a tail-recursive variant based on accumulating parameter

let rec sumA xs y = 
    match xs with
    | [] -> y
    | x::xs -> sumA xs (y+x)

sumA [1;2;3;4] 0

// Make the sum function continuation-based 
let rec sumC xs c =
    match xs with
    | [] -> c 0
    | x::xs -> sumC xs (fun v -> c(x+v))

sumC [1;2;3;4] id

// Problem 4

// 1
// Give a value of type T<int> <using all four constructors L, A, B and C.

type T<'a> = | L
             | A of 'a * T<'a>
             | B of 'a * T<'a> * T<'a>
             | C of 'a * T<'a> * T<'a> * T<'a>

let valueT = C(1,L, L,B(3,L, A(5,L)))

// A : A node consisting of a tuple with an int and a sub tree
// B : Consists of a triple with an int and 2 sub trees
// C : Consists of a quadruple with an int and 3 subtrees.
// L : Leaf

// 2

let rec f1 t = match t with
    | B(_, t1,t2) -> f1 t1 && f1 t2
    | L -> true
    | _ -> false;;

// Type for f1: T<'a> -> bool

let rec f2 t = match t with
    | L -> L
    | A(i,t) -> A(i, f2 t)
    | B(i,t1,t2) -> B(i, f2 t2, f2 t1)
    | C(i,t1,t2,t3) -> C(i, f2 t3, f2 t2, f2 t1);;

// Type for f2: t<'a> -> t<'b>

// Description: flips the branches around, so left branch goes to right and vice versa.

let rec f3 h = function
    | L -> L
    | A(i,t) -> A(h i, f3 h t)
    | B(i,t1,t2) -> B(h i, f3 h t1, f3 h t2)
    | C(i,t1,t2,t3) -> C(h i, f3 h t1, f3 h t2, f3 h t3);;

// Type for f3:  ('a -> 'b) -> t<'a> -> t<'b>

// Description for f3: Applies a function to the iteger i ('a).


