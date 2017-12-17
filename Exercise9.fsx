
// 9.3
let rec sumImp(m,n,x) =
     match n with
     | 0 -> m+x
     | n -> sumImp(m,n-1,x+m+n)

sumImp (5,4,0)

// 9.4

let rec lengthImp(x,y) =
    match x with
    |[] -> y
    |x::xs -> lengthImp(xs,y+1)

// 9.6
lengthImp([1;2;3;4;5],0)



let rec factC n k =
    match n with
    | 0 -> k 1
    | n -> factC (n-1) (fun v -> k(n*v))


let xs16 = List.init 1000000 (fun i -> 16)

#time

for i in xs16 do let _ = factC i id in ()


// Summer 2014 - Problem 1 (1-3)

let rec f n = function 
    | 0 -> 1
    | k when k>0 -> n * (f n (k-1))
    | _ -> failwith "illegal argument"

let rec g p f = function
    | [] -> []
    | x::xs when p x -> f x :: g p f xs
    | _::xs -> g p f xs;;
    
type T = 
    | A of int
    | B of string
    | C of T*T;;

let rec h = function
    | A n -> string n
    | B s -> s
    | C(t1,t2) -> h t1 + h t2;;

let sq = Seq.initInfinite (fun i -> 3*i);;

let k j = seq {for i in sq do
yield (i,i-j) };;
let xs = Seq.toList (Seq.take 4 sq);;
let ys = Seq.toList (Seq.take 4 (k 2));;

// 3.1

let rec fA n x k = 
    match k with
    | 0 -> x
    | k when k>0 -> fA n (x*n) (k-1)

fA 3 1 4

// 3.2

let rec fC n k = function
    | 0 -> k 1
    | x -> fC n (fun v -> k (n*v) ) (x-1)

fC 3 id 4

// Fall 2015 - Problem 2 (1-3)

// 2.1

let rec g1 p = function
    | x::xs when p x -> x :: g1 p xs
    | _ -> [];;

let rec g2 f h n x =
    match n with
    | _ when n<0 -> failwith "negative n is not allowed"
    | 0 -> x
    | n -> g2 h f (n-1) (f x);;


// 2.2 pt 1

let rec g1A x p y = 
    match x with
    | x::xs when p x -> g1A xs p (x::y)
    | x::xs -> g1A xs p y
    | [] -> y

let someList = [1;2;3;4;5;6;7]

g1A someList (fun x -> x > 3) []

// 2.2 pt 2

let rec g1C x p k =
    match x with
    | x::xs when p x -> g1C xs p (fun v -> k(x::v))
    | x::xs -> g1C xs p k
    | [] -> k []

g1C someList (fun x -> x > 3) id 
