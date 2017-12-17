// December 16
type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list
let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30); ("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;

// 1.1
let rec inv = function
    | [] -> true
    | [(n,e,p)] -> p>=0 
    | (n,e,p)::(n1,e1,p1)::sb -> p >= p1 && inv ((n1,e1,p1)::sb)

inv sb

// 1.2
let rec insert sb (x1,y1,z1) =
    match sb with
    | [] when z1>0-> [(x1,y1,z1)]
    | (x,y,z)::tail when z <= z1 -> (x1,y1,z1)::(x,y,z)::tail
    | (x,y,z)::tail -> (x,y,z)::insert tail (x1,y1,z1)

insert sb ("Kirke", "June Fishing", 35)

// 1.3
let rec get(n,sb) = 
    match sb with
    | [] -> []
    | (n1,e1,p1)::sb1 when n=n1 -> (e1,p1)::get(n,sb1)
    | _::sb1 -> get(n,sb1)

get ("Joe", sb)

// 1.4
// ???????????
let rec top k = function
    | [] -> if k = 0 then Some [] else None
    | sb when k = 0 -> Some []
    | s::sb -> match top (k-1) sb with
                | None -> None
                | Some sb' -> Some(s::sb');;
 
top 2 sb
// 2.1
let rec replace a b xs =
    match xs with
    | [] -> []
    | x::tail when x = a -> b::(replace a b tail)
    | x::tail -> x::(replace a b tail)

replace 2 7 [1; 2; 3; 2; 4]

// 2.2

// int -> int -> int list -> int list

// 2.3
// 2.1 made into a tail-recursive function
let rec replaceTR res a b xs =
    match xs with
    | x::xs when x = a -> replaceTR (b::res) a b xs
    | x::xs -> replaceTR (x::res) a b xs
    | [] -> List.rev res

replaceTR [] 2 7 [1; 2; 3; 2; 4]

// 3.1

let pos = Seq.initInfinite (fun i -> i+1) ;;
let seq1 = seq { yield (0,0)
                 for i in pos do
                    yield (i,i)
                    yield (-i,-i) }

let val1 = Seq.take 5 seq1;;
Seq.toList val1
// [(0, 0); (1, 1); (-1, -1); (2, 2); (-2, -2)]

// 3.2

let nat = Seq.initInfinite id;;
let seq2 = seq { for i in nat do
                    yield (i,0)
                    for j in [1 .. i] do
                        yield (i,j) }

let val2 = Seq.toList(Seq.take 10 seq2);;

//  [(0, 0); (1, 0); (1, 1); (2, 0); (2, 1); (2, 2); (3, 0); (3, 1); (3, 2); (3, 3)]


// 4.1

// 'a: bool          'b: int list

type Tree<'a,'b> = | A of 'a | B of 'b
                   | Node of Tree<'a,'b> * Tree<'a,'b>;;

let val1 = Node (Node (A true, B [1;2]), Node(A false, B [2;3]))
let val2 = Node (Node (A true, B [1;2]), B [4;5])
let val3 = A false

// 4.2 
let rec count t =
    match t with
    | Node (a,b) -> count a + count b 
    | B b -> 0
    | A a -> 1

count val1

// 4.3

let rec subset a a' b b' t =
    match t with
    | Node (a2,b2) -> Node (subset a a' b b' a2,  subset a a' b b' b2)
    | A a1 -> if a=a1 then A a' else A a1
    | B b1 -> if b=b1 then B b' else B b1

subset true false [1;2] [5;6] val1

// 4.4 

// g: Tree <'a,'b> -> Tree <'b,'a>

let rec g = function
    | Node(t1,t2) -> Node(g t2, g t1)
    | leaf -> leaf;;

g val1 // = Node (Node (B [2; 3],A false),Node (B [1; 2],A true))


// f: Tree<'a,'b> -> (['a],['b])

let rec f = function
    | A a -> ([a],[])
    | B b -> ([], [b])
    | Node(t1,t2) -> let (xs1,ys1) = f t1
                     let (xs2,ys2) = f t2
                     (xs1@xs2, ys1@ys2);;
                 
f val1

// Puts the A values into a list in the first index of a tuple, and the values
// of B into a list into the second index of the tuple.

// 4.5
// wtf
let rec fC t k = 
    match t with
    | A a           -> k ([a], [])
    | B b           -> k ([], [b])
    | Node (t1,t2)  -> fC t1 (fun (v1,v2) -> k(fC t2 (fun (v3,v4) -> ((v1 @ v3),(v2 @ v4)));;

fC val1 id



let rec g1C x p k =
    match x with
    | x::xs when p x -> g1C xs p (fun v -> k(x::v))
    | x::xs -> g1C xs p k
    | [] -> k []


