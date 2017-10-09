open System.Security.Cryptography.X509Certificates

// Exercises for September 8th

// Exercise 1
let rec adder = function
    | 0 | 1 -> 1
    | n -> n + adder(n-1)

(adder 4)
;;

// Exercise 2
let rec sum = function
    | (m,0) -> m
    | (m,n) -> m + n + sum(m,n-1)
    ;;
sum(4,3)


// Exercise 3

let rec bin = function
    | (n,0) -> 1
    | (n,k) when n=k -> 1
    | (n,k) -> bin(n-1,k-1) + bin(n-1, k);;

bin(0,2)

// Exercise 4

let rec multiplicity(x,ys) = 
    match ys with
    | ([]) -> 0
    | y::tail when x = y -> 1 + multiplicity(x,tail)
    | y::tail -> multiplicity(x,tail)
    ;;
multiplicity(2,[2; 4; 2; 10; 1; 2; 2])


// Exercise 5

let rec mulC(x,ys) = 
    match ys with
    |[] -> []
    |y::tail -> x*y::mulC(x,tail)
mulC(2,[4; 10; 1])

// Exercise 6

let rec addE(xs,ys) = 
    match (xs,ys) with
    | (xs,[]) -> xs
    | ([],ys) -> ys
    | (x::xtail,y::ytail) -> x+y::addE(xtail,ytail);;

addE([1;2;3;4],[5;6])

// Exercise 7
// (a)
let mulX(Q) = 0::Q

mulX([2;0;0;1])

// (b)
let rec mul = function
    | ([],Q) -> []
    // | (a::atail,Q) -> a*Q + a:: mulX(mulC(atail,Q))
    | (a::atail,Q) -> addE(mulC(a,Q), mulX(mul(atail,Q)))

mul([2;3;0;1], [1;2;3])



