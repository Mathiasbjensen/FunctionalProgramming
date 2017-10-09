open System.Security.Cryptography.X509Certificates

// Week 3

// 1
let rec repeat(s,n) = 
    match n with
    | 0 -> ""
    | n -> s + repeat(s,n-1)
    
//repeat("ab",4)

// 2
let rec f(s1,s2,n) =
    match n with
    | 0 -> ""
    | n -> s1 + "\n" + f(s2,s1,n-1)

//f("ab","cd",4)

// 3

let viz(m,n) =
    f(repeat("XO",m) , repeat("OX",m), n)
    
viz(4,5)

// 2.1

(* [x0+i^(2^0) ; x1 + i^(2^1) ; x2 + i^(2^2) ; xn + i^(2^n)] *)

// 3.1

type Rel<'a,'b when 'a: equality> = ('a * 'b list) list

let rel: Rel<int,string> = [(1, ["a"; "b"; "c"]); (4,["b"; "e"])];;

let rec apply = function
    | (_,[]) -> []
    | (x,(a,ys)::ytail) when x = a -> ys
    | (x,(a,ys)::ytail) -> apply(x,ytail)

apply(1, rel)

// 3.2

let rec existsInList = function
    | (_,[]) -> false
    | (x,ys::ytail) -> if x = ys then true else existsInList(x,ytail)

let rec inRelation(x,y,rel) =
    match y with
    | y -> if existsInList(y,apply(x,rel)) = true then true else false

inRelation(4, "e", rel)

// 3.3

let rec insert(x,y,rel) = 
    match rel with
    | [] -> []
    | (a,ys)::reltail -> if x=a then (a,y::ys)::reltail else (a,ys)::insert(x,y,reltail);;
    
insert(3,"c",[(1,["a"]); (2,["b"])])


