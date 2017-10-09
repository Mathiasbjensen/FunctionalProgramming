open System.Xml.Xsl

// 2.1
let f = function
    | n when n % 5 = 0 -> false
    | n when n % 2 = 0 -> true
    | n when n % 3 = 0 -> true
    | _ -> false ;;

f(12)

// 2.2
let rec pow(s,n) = 
    match n with
    | 0 -> ""
    | n -> s + pow(s,n-1)

pow("s",3)

// 4.3
let rec evenN(n) = 
    match n with
    | 0 -> []
    | n -> evenN(n-1) @ [n*2]
  
evenN(4)

// 4.8
let rec split = function
    | [] -> ([],[])
    | x::y::tail -> let (x1,y1) = split(tail)
                    (x::x1,y::y1)
    | [x] -> ([x],[])
    ;;
    
split([1;2;3;4;5;6;7])

// 4.9
let rec zip = function
    | ([],[])     -> ([])
    | (_,[])      -> failwith ("Not of equal length")
    | ([],_)      -> failwith ("Not of equal length")
    | x::xtail,y::ytail    -> (x,y)::zip(xtail,ytail)


zip([1;2;3;4],[5;6;7;8])

// 4.12
// sum tager imod en funktion (p) og en liste med ints (xs).
let rec sum = function
    | (p,[]) -> 0
    | (p, x::xs) when p(x) -> x + sum(p,xs)
    | (p, x::xs) -> 0 + sum(p,xs)
    

sum((fun x->x<0),[1;2;3;-6;-8;-1])
// sum(p,xs)

// 4.11 - 1
let rec count(x,ys) =
    match ys with
    | ([])               -> 0
    | y::tail when y = x -> 1 + count(x,tail)
    | y::tail            -> count(x,tail)

count(1,[1;2;1;2;1])

// 4.11 - 2
//let rec insert(xs,x) =
//    match xs with
//    | [] -> []
//    |












// Merge stuff
//let rec merge(x,y) =
//    match(x,y) with
//    |([],y) -> y
//    |(x,[]) -> x
 //   |(x::xtail,y) when x < y -> x + merge(xtail,y)
 //   |(x,y::tail) when y > x -> 



 // ---- Week 3 (exam set)

 // 1

let rec repeat(