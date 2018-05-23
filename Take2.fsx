open System.Security.Cryptography.X509Certificates
// MAGTER

// --- Week 1 ---

// 1
let rec f = function
    | 0 -> 0
    | n -> n + f (n-1)

f 10

// 2
let rec sum = function
    | (m,0) -> m
    | (m,n) -> m + n + sum(m,n-1)

sum(2,4)

// 3  Magtede ej
let rec bin = function
    | (n,0) -> 1
    | (n,k) when n=k -> 1
    | (n,k) when n <> 0 && k <> 0 &&  n>k -> bin(n-1,k-1) + bin(n-1,k)

bin(0,2)

// 4
let rec multiplicity(x,ys) =
    match ys with
    | ([]) -> 0
    | y::tail when x = y  -> 1+ multiplicity(x,tail)
    | y::tail -> multiplicity(x,tail)

multiplicity(2, [2; 4; 2; 10; 1; 2])

// 5

let rec mulC n = function
    | []    -> []
    | x::xs -> (x*n)::(mulC n xs)

mulC 2 [4; 10; 1]

// 6 
let rec addE = function
    | ([],[]) -> []
    | (x,[])    -> x
    | ([],y)    -> y
    | (x::xs,y::ys) -> x+y::addE(xs,ys)

addE([1; 2; 3], [4; 5; 6])
addE([1; 2], [3; 4; 5; 6])
addE([1; 2; 3; 4], [5; 6])

// 7.a
let mulX(Q) = 0::Q

// 7.b
let rec mul = function
    | (Q,_)         -> []
    | (Q,a::atail)  -> addE(mulC(a,Q),mulX(mul(atail,Q)))

mul([2;3;0;1],[1;2;3])

// 7.c

let rec txt = function
    | (_,[])                    -> ""
    | (n,a::[])                 -> string a + "x^" + (string n)
    | (n,0::atail)               -> txt(n+1,atail)
    | (n,a::atail) when n < 2   -> string a + "+" + txt(n+1,atail)
    | (n,a::atail)              -> string a + "x^" + (string n) + "+" + txt(n+1,atail)
    //| a::atail  -> a "+" + txt atail

txt (0,[2;3;0;1])

let rec txt = function
    | (_,[])                    -> ""
    | (n,a::[])                 -> string a
    | (n,a::atail)              -> string a + "x^" + (string n) + "+" + txt(1,atail)



//------------------------------------------------------------------------------------------------
// ***** Week 2 *****

// 2.1

let f n = (n % 2 = 0 || n % 3 = 0) && (n % 5 <> 0)

// 2.2
let rec pow = function
    | (_,0) -> 1
    | (s,n) -> s*pow(s,n-1)

// 4.3
let rec evenN = function
    | 0 -> []
    | n -> evenN(n-1)@[n*2]

// 4.8
let rec split = function
    | [] -> ([],[])
    | x::xs::tail -> let (x1,xs1) = (split tail)
                     (x::x1,xs::xs1)
    | [x] -> ([x],[])

split [0;1;2;3;4;5;6;7]

// 4.9
let rec zip = function
    | ([],[])           -> []
    | (x,[])            -> failwith "not same length"
    | ([],y)            -> failwith "not same length"
    | (x::xs,y::ys)     -> (x,y)::zip(xs,ys)

zip ([0;2;4;6;8;10],[1;3;5;7;9;11])

// 4.12
let rec sum p = function
    | []    -> 0
    | x::xs -> if p x then x + (sum p xs) else sum p xs

let p x = x > 2
sum p [1;2;3;4]

// 4.11
// 4.11.1
let rec count = function
    | (x,[])        -> 0
    | (x,xs::tail)  -> if x = xs then 1 + count(x,tail) else count(x,tail)

count(3,[1;2;3;3;3;3;5])

//4.11.2
let rec insert = function
    | ([],_)                    -> []
    | (xs::tail,x) when x <= xs -> x::xs::tail
    | (xs::tail,x)              -> xs::(insert(tail,x))

insert([1;2;3;5;6],4)

// 4.11.3
let rec intersect = function
    | ([],[])           -> []
    | (xs,[])           -> xs
    | ([],_)            -> []
    | (x::xs,y::ys)        -> if count(x,y::ys) > 0 then x::(intersect(xs,ys))
                                else intersect(xs,y::ys)

                  
intersect([1;1;1;2;2],[1;1;2;4])

// 4.12.4
let rec plus = function
    | ([],[])                   -> []
    | (x,[])                    -> x
    | ([],y)                    -> y
    | (x::xs,y::ys) when x=y   -> x::y::plus(xs,ys)
    | (x::xs,y::ys)             -> if x < y  then x::plus(xs,y::ys)
                                    else y::plus(x::xs,ys)

plus([1;1;2],[1;2;4])

// ---------------------------------------------------
// ***** Week 3 *****

// 1.1
let rec repeat s = function
    | 0     -> ""
    | n     -> s + repeat s (n-1)

repeat "ab" 3

// 1.2

let rec f s1 s2 = function
    | 0     -> ""
    | n     -> s1 + "\n" + f s2 s1 (n-1)

f "XO" "OX" 3

// 1.3
let rec viz m n = f (repeat "XO" m) (repeat "OX" m) n
viz 4 5

// 2.1
let rec f i = function
    | [] -> []
    | x::xs -> (x+i)::f (i*i) xs;;

f 2 [0;0;0;0;0;]

type Rel<'a,'b when 'a: equality> = ('a * 'b list) list
let rel: Rel<int,string> = [(1, ["a"; "b"; "c"]); (4,["b"; "e"])];;


// 3.1

let rec apply x = function
    | []    -> []
    | (r,rel)::rs -> if r = x then rel else apply x rs

apply 1 rel

// 3.2
let rec inRelation x y = function
    | []    -> false
    | (r,rel)::rs -> if r = x && List.contains y rel then true else inRelation x y rs

inRelation 4 "b" rel

// 3.3

let rec insert x y = function
    | []                        -> []
    | (r,rel)::rs when x = r    -> (r,y::rel)::(insert x y rs)
    | (r,rel)::rs               -> (r,rel)::(insert x y rs)



insert 4 "c" rel

// 3.4

let rec toRelAux a = function
    | []            -> false
    | (r,rel)::rs   -> if r = a then true else toRelAux a rs

toRelAux 1 rel

let rec toRel acc = function
    | []            -> []
    | (r,rel)::rs   ->;;


// 4.1

let rec repeatList xs = function
    | 0         -> []
    | n         -> xs @ repeatList xs (n-1)

repeatList [1;2;3] 4

// 4.2

let rec merge = function
    | ([],[])           -> []
    | (xs,[])           -> xs
    | ([],ys)           -> ys
    | (x::xs,y::ys)     -> x::y::merge(xs,ys)

merge([1; 2; 3; 4], [5; 6])
;;
// --------------------------------------------------------------

// ***** Week 5 *****

// 3.5
type Solution =
    | TwoRoots of float * float
    | OneRoot of float
    | NoRoots

// 5.2
// without fold:
let rec revrev = function
    | []      -> []
    | x::xs     -> List.rev ((List.rev x)::revrev xs)

revrev [[1;2];[3;4;5]]

// with fold:
let revrevList xs =  List.fold (fun rs x -> x::rs) [] xs
let revrev2 xs = List.fold (fun rs x -> revrevList (x)::rs) [] xs
revrev2 [[1;2];[3;4;5]]
// val it : int list list = [[5; 4; 3]; [2; 1]]


// 5.3
// without fold:
let rec sum = function
    | (p,[])    -> 0
    | (p,x::xs) -> if p x then x + sum(p,xs) else sum(p,xs)

let p x = x > 0
sum (p,[1;2;3;-4;-5])

// with fold:
let sum(p,xs) = List.fold (fun y x -> if p x then y + x else y) 0 xs

sum(p,[1;2;3;-4;-5])

// ---------------------------------------------------------------------
// ***** WEEK 6 *****

// Fall 2013, 1.
type Multiset<'a when 'a : equality> = ('a * int) list;;
// 1.1

let rec invAux e ms = List.exists (fun (a,_) -> a = e) ms

let rec inv = function
    | []            -> true
    | (e,n)::xs     -> if invAux e xs then false else inv xs

inv [("b",3); ("a",5); ("d",1)]

// 1.2
let rec insert e n = function
    | []            -> []
    | (e1,n1)::xs   -> if e1 = e then (e1,n1+n)::xs else (e1,n1)::insert e n xs 

insert "a" 2 [("b",3); ("a",5); ("d",1)]

// 1.3
let rec numberOf e = function
    | []            -> 0
    | (e1,n)::xs    -> if e1 = e then n else numberOf e xs

numberOf "a" [("b",3); ("a",5); ("d",1)]

// 1.4
let rec delete e = function
    | []            -> []
    | (e1,n)::xs    -> if e1 = e then (e1,n-1)::xs else (e1,n)::delete e xs

delete "a" [("b",3); ("a",5); ("d",1)]

// 1.5

let rec removeAux e = function
    | []            -> []
    | (e1,n)::xs    -> if e1 = e then xs else (e1,n)::removeAux e xs


let rec union = function
    | ([],[])                   -> []
    | (xs,[])                   -> []
    | ([],ys)                -> ys
    | ((e1,n1)::xs,ys)          -> (e1,n1+(numberOf e1 ys))::union (xs,(removeAux e1 ys))

union ([("b",3); ("a",5); ("d",1)], [("a",3); ("b",4); ("c",2)])

// 1.6 change from list to maps.
type MultisetMap<'a when 'a : comparison> = Map<'a,int>;;

let test = Map.ofList [("b",3); ("a",5); ("d",1)]
let inv ms = Map.forall (fun x (e,n) -> n > 0) ms

inv (Map.ofList [("b",3); ("a",5); ("d",1)])

// HER MAGTER DU INTET


// ***** WEEK 8 *****

//type FileSys = Element list 
//    and Element =
//    | File of string * string
//    | Dir of string * FileSys

let d1 = Dir("d1",[File("a1","java");
                    Dir("d2", [File("a2","fsx");
                                Dir("d3", [File("a3","fs")])]);
                    File("a4","fsx");
                    Dir("d3", [File("a5","pdf")])]);;
// List of names:
let rec nameFileSys = function
    | []            -> []
    | e::es         -> (nameElement e) @ (nameFileSys es)
and nameElement = function
    | File (s,ext)      -> [string s + "." + string ext]
    | Dir(s,fs)         -> s::(nameFileSys fs)

nameElement d1

// Search:
let rec searchFileSys ext = function
    | []            -> []
    | e::es         -> (searchElement ext e) @ (searchFileSys ext es)
and searchElement ext = function
    | File (s,ext1)      -> if ext1=ext then [s] else []
    | Dir(s,fs)         -> (searchFileSys ext fs)

searchElement "fsx" d1

// longNames:

let rec longNamesFileSys = function
    | [] -> Set.empty
    | e::es -> Set.union (longNamesElement e) (longNamesFileSys es) 
and longNamesElement = function
    | File (s,s1) ->  set [s+"."+s1]
    | Dir (s,fs) -> Set.map ((+) (s + "\\") (longNamesFileSys fs)

longNamesElement d1;;

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
// Ikke done...
let rec intpInstr = function
    | (_,[])                -> []
    | (ADD,x::xs::rst)      -> x+xs::rst
    | (SUB,x::xs::rst)      -> x-xs::rst
    | (MULT,x::xs::rst)     -> x*xs::rst
    | (DIV,x::xs::rst)      -> x/xs::rst
    | (SIN,x::xs)           -> sin(x)::xs
    | (COS,x::xs)           -> cos(x)::xs
    | (LOG,x::xs)           -> log(x)::xs
    | (EXP,x::xs)           -> exp(x)::xs
    | (PUSH a,x)            -> a::x














// !!!!!!!!!!!!!!!! MANDATORY ASSIGNMENTS !!!!!!!!!!!!!!!!

// --- ASSIGNMENT 1 ---
type Member = string * int * string list
type Register = (string * Member) list

let reg = [("Hans",("12345678",1980,["soccer";"jazz";"eating"])) ; 
           ("Mathias",("29675229",1994,["soccer";"running";"drinking"]));
           ("Anne",("12345679",1982,["jazz";"skod";"soccer"]));
           ("Lone",("87654321",1832, ["food";"cleaning"]))]

let p1 = function
    | (_,yb,ths) -> yb >= 1982 && List.contains "soccer" ths 
                                && List.contains "jazz" ths

let p2 = function
    | (_,yb,ths) -> yb >= 1982 && (List.contains "soccer" ths 
                                || List.contains "jazz" ths)

let rec extractInterested p = function
    | []        -> []
    | (name,(no,yb,ths))::xs     -> if p (no,yb,ths) then name::extractInterested p xs
                                    else extractInterested p xs
extractInterested p1 reg




// --- ASSIGNMENT 3 ---
type Name = string 
type Flow = int // can be assumed positive in below questions
type River = R of Name * Flow * Tributaries 
and Tributaries = River list

// 1
let riv3 = R("R3",8,[])
let riv = R ("R",10,[R ("R1",5,[]);R ("R2",15,[R ("R4",2,[])]);riv3])


// 2
let rec containsAux n = function
    | []    -> false
    | (r::rs)  -> contains n r || containsAux n rs
and contains n = function
    | R (name,f,trib)   -> n=name || containsAux n trib

contains "R2" riv

// 3
let rec allNamesAux = function
    | []        -> []
    | (r::rs)   -> (allNames r) @ (allNamesAux rs)
and allNames = function
    | R (name,_,trib)   -> name::(allNamesAux trib)

allNames riv

// 4
let rec totalFlowAux = function
    | []        -> 0
    | (r::rs)   -> (totalFlow r) + (totalFlowAux rs)
and totalFlow = function
    | R (_,f,trib)   -> f + (totalFlowAux trib)

totalFlow riv

// 5

let rec helpMainSource = function
    | (n1,f1)::(n2,f2)::t   -> if f1>f2 then helpMainSource ((n1,f1)::t) else helpMainSource ((n2,f2)::t)
    | [(n1,f1)]             -> (n1,f1);;

let rec mainSourceAux = function
    | []        -> ["",0]
    | (r::rs)   -> mainSource r :: mainSourceAux rs
and mainSource = function
    | R (name,flow,trib)    -> helpMainSource((name,flow) :: (mainSourceAux trib));;

mainSource riv
 
// 6
let rec tryInsertAux = function
    | [] -> []
    | t1::tail -> tryInsertHelp t1 @ tryInsertAux tail
and tryInsertHelp = function
    | R (n1,f,trib) -> (R (n1,f,trib))::(tryInsertAux trib)
       
let tryInsert n t r =
    match List.tryFind (fun (R (x,_,_)) -> x = n) (tryInsertHelp r) with
        |Some(R (x1,t1,trib)) -> Some(R (x1,t1,t::trib))
        |None -> None

tryInsert "R2" (R ("R5",20,[])) riv


let rec nameFileSys = function
    | []            -> []
    | e::es         -> (nameElement e) @ (nameFileSys es)
and nameElement = function
    | File (s,ext)      -> [string s + "." + string ext]
    | Dir(s,fs)         -> s::(nameFileSys fs)









// *** EXAMS ***

// --- Summer-2014 ---

// 1.1
let rec f n = function  | 0 -> 1
                        | k when k>0 -> n * (f n (k-1))
                        | _ -> failwith "illegal argument";;
// er blot power funktionen

type T =    | A of int
            | B of string
            | C of T*T;;

let rec h = function
        | A n -> string n
        | B s -> s
        | C(t1,t2) -> h t1 + h t2;;

h (C (A 2, B "a"))

let sq = Seq.initInfinite (fun i -> 3*i);;

Seq.toList(Seq.take 5 sq)

let k j = seq {for i in sq do
                    yield (i,i-j)}

k 2 // yields a sequence of tuples containging i and i-j. (0,-2); (3,1); ....

let xs = Seq.toList (Seq.take 4 sq);;
let ys = Seq.toList (Seq.take 4 (k 2));;

// 1.3 - making f tailrecursive
let rec fA n a = function  
    | 0 -> a
    | k when k>0 -> fA n (a*n) (k-1) 
    | _ -> failwith "illegal argument"

fA 2 1 3

// - Makking f continuation based
let rec fC n c = function
    | 0     -> c 1
    | k when k>0 -> fC n (fun x -> c(x*n)) (k-1)
    |  _ -> failwith "illegal argument"

fC 2 id 3


// 2.1
let rec ordered = function
    | []        -> true
    | x::y::res -> if x <= y then ordered (y::res) else false
    | x         -> true

// 2.2
let rec smallerThanAll x = function
    | []            -> true
    | y::ys         -> if x<y then smallerThanAll x ys else false



// 2.3
let rec insertBefore p x = function
    | []        -> []
    | xs::tail  -> if p xs then x::xs::tail else xs::(insertBefore p x tail)

let p x = x=5
insertBefore p 10 [1;2;3;4;5;6;7;8]

// 2.4
//type Sex =  | M // male
//            | F // female

let sexToString = function
    | M -> "Male"
    | F -> "Female";;

sexToString M

// 2.5
let rec replicate str = function
    | 0     -> ""
    | n     -> str + replicate str (n-1)
    | _     -> failwith "argument n is negative";;


// 3.1
type Name = string
type Sex =  | M // male
            | F // female
type YearOfBirth = int;;
type FamilyTree = P of Name * Sex * YearOfBirth * Children
and Children = FamilyTree list;;

// Helping function that checks if all children are younger and if the children are ordered,
// sort of like in problem 2.
let rec helpisWF y = function
    | []                                -> true
    | P (_,_,y1,_)::P (n2,s2,y2,c2)::xs -> if y<y1 && y1<y2 then helpisWF y ((P (n2,s2,y2,c2))::xs) else false
    | [P (_,_,y1,_)]                    -> if y<y1 then true else false

let rec isWFAux = function
    | []    -> true
    | x::xs -> isWF x && isWFAux xs
and isWF = function
    | P (_, _, y, c)  -> helpisWF y c && isWFAux c

let famTreeTest = P ("Søren", M,  1966, [P ("Mathias", M, 1994, []); P ("Simone", F, 1996, [])])

let ft = P ("Larry", M, 1920, [P ("May", F, 1945,[P("Fred", M, 1970,[]);P ("Joan", F, 1975,[])]); 
                                P("Joe", M, 1950,[P("Stanley", M, 1975,[]);P("Mary", F, 1980,[P("Peter", M,2005,[]);P("Bob", M, 2008,[]);P("Eve",F,2010,[])]);P("Jane",F,1985,[])]); 
                                P ("Paul", M, 1955,[])])

isWF famTreeTest

// 3.2

let makePerson = function
    | (n,s,y)   -> P (n, s, y, [])

makePerson ("Anni", F, 1970)

// 3.3

let rec insertChildOfHelper n c = function
    | P (n1,s,y,t) -> if n = n1 then P (n1,s,y,c::t) else P (n1,s,y,insertChildOfAux n c t )
and insertChildOfAux n c = function
    | x::xs -> (insertChildOfHelper n c x)::(insertChildOfAux n c  xs)
    | []    -> []
and insertChildOf n c f =
    let t = insertChildOfHelper n c f
    match  isWF t with
    | true  -> Some(t)
    | false -> None

insertChildOf "Bob" (P ("skod",M,2010,[])) ft


// 3.4

let rec getNamesAux = function
    | []                    -> ""
    | (P (n,_,_,_))::xs     -> n + ", " + getNamesAux  xs


let rec findAux n a = function
    | []    -> ""
    | x::xs -> find n a x + findAux n a xs
and find n a = function
    | P (n1,s,y,c)  -> if n=n1 then string(s)+", "+string(y) + ": "+ getNamesAux c + string(a) else findAux n (a+1) c 

find "Mary" 0 ft

// 3.5



let rec indent = function
    | 0 -> ""
    | n -> " " + indent (n-1);;


let rec toStringAux n a = function
    | []                -> ""
    | x::xs             -> toStringHelp n a x + toStringAux n a xs
and toStringHelp n a = function
        | P (name,s,y,c) -> indent (n*a) + string(name) + " " + sexToString s + " " + string(y) + "\n"  + toStringAux n (a+1) c
and toString n f = toStringHelp n 0 f


toString 6 ft

let ft = P ("Larry", M, 1920, [P ("May", F, 1945,[P("Fred", M, 1970,[]);P ("Joan", F, 1975,[])]); 
                                P("Joe", M, 1950,[P("Stanley", M, 1975,[]);P("Mary", F, 1980,[P("Peter", M,2005,[]);P("Bob", M, 2008,[]);P("Eve",F,2010,[])]);P("Jane",F,1985,[])]); 
                                P ("Paul", M, 1955,[])])


// Extra shit
//  count how many of a sex there are in the family tree:

let rec noOfSexAux s = function
    | []            -> 0
    | x::xs         -> noOfSex s x + noOfSexAux s xs
and noOfSex s = function
    | P (_,s1,_,c)   -> if s1 = s then 1 + noOfSexAux s c else noOfSexAux s c

noOfSex M ft


(*let rec depthAux = function
    | []            -> 0
    | x::xs         -> depth x + depthAux xs
and depth = function
    | P (n,s,y,c) when c <> []  -> 1 + depthAux c
    | P (n,s,y,c)               -> depthAux c

depth ft

let rec depthTestAux = function
    | []        -> ""
    | x::xs 
*)







// --- Summer-2015 ---

// 1.1
let rec repeat s = function
    | 0     -> ""
    | n     -> s + repeat s (n-1)

// 1.4 - Tail recursive
let rec repeatA (acc : string) s = function
    | 0     -> acc
    | n     -> repeatA (s + acc) s (n-1)

repeatA "" "ab" 4

// 1.4 - Continuation based
let rec repeatC s c = function
    | 0     -> c ""
    | n     -> repeatC s (fun x -> c(s + x)) (n-1) 

repeatC "ab" id 4

// 2.1
let rec mixMap f x y = 
    match (x,y) with
    | ([],[])         -> []
    | (x::xs,y::ys)   -> (f x y)::(mixMap f xs ys)

let a1 = [2;3;4;5]
let a2 = [10;20;30;40]
let f1 x y = x*y

mixMap f1 a1 a2

// 2.2
let rec unMixMapAux f g c = function
    | []                        -> []
    | (x,_)::res when c = 0     -> f x::unMixMapAux f g c res
    | (_,y)::res when c = 1     -> g y::unMixMapAux f g c res

let rec unMixMap f g xs = ((unMixMapAux f g 0 xs),(unMixMapAux f g 1 xs))

let f1 x = x*2
let g1 x = x*4 
unMixMap f1 g1 [(1,2);(3,4);(5,6)]

// 3.1
type Tree<'a> = Lf | Br of Tree<'a> * 'a * Tree<'a>;;
let t = Br(Br(Br(Lf,1,Lf),2,Br(Lf,3,Lf)),4,Br(Br(Lf,5,Lf),6,Br(Lf,7,Lf)));;

let rec reflect = function
    | Lf _          -> Lf
    | Br (tl,x,tr)  -> Br((reflect tr), x, (reflect tl))

reflect t

// 3.2
// FORKERT AHJSUDIAHUDFIAHDOFGASYUIODGSYAuidgasyuidfgasuyidgv uis
let rec accAux c = function
    | 0 -> c + 0
    | n -> c + n

let rec accumulate c = function
    | Lf _          -> Lf
    | Br (tl,x,tr)  -> Br((accumulate (c+x) tl), c+x, (accumulate (c+x) tr))

accumulate 0 t



// traversing from root first -> tl -> tr
let rec preOrder = function
    | Lf _          -> []
    | Br (tl,x,tr) -> x:: (preOrder tl) @ (preOrder tr)

preOrder t



// EXAM: Fall 2016

type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list

let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)]
// 1.1
let rec inv = function
    | []            -> true
    | [(n,e,p)]  -> p>=0
    | (n,e,p)::(n1,e1,p1)::xs -> p>=p1 && inv ((n1,e1,p1)::xs)

inv sb

// 1.2
let rec insert (n1,e1,p1) = function
    | []        -> []
    | (n,e,p)::xs   -> if p1>=p then (n1,e1,p1)::(n,e,p)::xs else (n,e,p)::(insert (n1,e1,p1) xs)

insert ("Hans",  "May Fishing", 33) sb

// 1.3
let rec get = function
    | (n,[])                -> []
    | (n,(n1,e1,p1)::sb) when n=n1   -> (e1,p1)::get (n,sb)
    | (n,(n1,e1,p1)::sb)             -> get(n,sb)

get ("Joe",sb)

// 1.4 - Michaels some/none - ok hjernedødt
let rec top n = function
                | _ when n=0 -> Some []
                | []         -> None      
                | s::sb      -> match top (n-1) sb with 
                                | None -> None
                                | Some res -> Some(s::res)  
top 2 sb

// 2.1
let rec replace a b = function
    | []    -> []
    | x::xs when x=a    -> b::(replace a b xs) (* 1 *)
    | x::xs             -> x::(replace a b xs) (* 2 *)

replace 2 7 [1; 2; 3; 2; 4]

// 2.2

// The most general type ust have the form: t1 -> t2 -> t3 -> t4, for some types t1, t2, t3, t4, due to the form of the declaration, 
// where a:t1, b:t2, t3 = t1 list due to x=a in (* 1 *)
// Furthermore, t2 = t1 and t4 = t1 list due to b::... in (* 1 *) and x::... in (* 2 *)  
// The only constraint on t1 is that it must support equality. 
// Hence, most general type is: 'a -> 'a -> 'a list -> 'a list when 'a: equality

// Replace is not tail-recursive, because when the recursive call in (* 1 *) terminates, then the cons operation b:: ... remains to be executed, 
// that is, this recursive call is not a tail call. Similarly for (* 2 *)

//2.3 tail-recursive liste
let rec replace a b res = function
    | []    -> List.rev res
    | x::xs when a = x  -> replace a b (b::res) xs
    | x::xs             -> replace a b (x::res) xs

replace 2 7 [] [1; 2; 3; 2; 4]

//3.1
let pos = Seq.initInfinite (fun i -> i+1) ;;

let seq1 = seq {yield (0,0) 
                for i in pos do
                    yield (i,i)
                    yield (-i,-i)}

let val1 = Seq.take 5 seq1;;
val1




// Exam May 2017

//Problem 1

// 1
let rec repeatList xs = function
    | 0     -> []
    | n     -> xs@repeatList xs (n-1)

repeatList [1;2] 3
// val it : int list = [1; 2; 1; 2; 1; 2]
// Hvis man brugte cons :: giver det [[1;2];[1;2];[1;2]]

// 2
let rec merge = function
    |(x::xs,y::ys)  -> x::y::merge(xs,ys)
    |([],y)     -> y
    |(x,[])     -> x

merge([1; 2; 3; 4], [5; 6])

// Problem 2

//1 
let rec f = function
| 0 -> [0]
| i when i>0 -> i::g(i-1)
| _ -> failwith "Negative argument"
and g = function
| 0 -> []
| n -> f(n-1)

f 5
// val it : int list = [5; 3; 1]
// int -> intlist

let h s k = seq { for a in s do
                     yield k a }

h (seq [1;2;3;4]) (fun i -> i+10)

// val it : seq<int> = seq [11; 12; 13; 14]
// val h : s:seq<'a> -> k:('a -> 'b) -> seq<'b>

// 2.2
// tail recursve - accumulating parameter
let rec sumA a = function
    | []    -> a
    | x::rest   -> sumA(a+x) rest

sumA 0 [1;3;5]

// tail recursive - continuation based
let rec sumC c = function
    | []    -> c 0
    | x::rest   -> sumC (fun res -> c(res+x)) rest

sumC id [1;3;5]

// Problem 3
type Article = string
type Amount = int
type Price = int
type Desc = Amount*Price
type Stock = (Article*Desc) list
let st = [("a1",(100,10)); ("a2",(50,20)); ("a3",(25,40))]

// 3.1
let rec stockVal = function
    | []                -> 0
    | (a,(n,p))::tail   -> (n*p)+stockVal tail

stockVal st

// 3.2
let rec inv = function
    | []                -> true
    | (a,(n,p))::tail   -> if n>0 && p>0 && not (List.exists (fun (a1,(n1,p1)) -> a1=a) tail) then inv tail else false
inv st


// 3.3
type Order = Article*Amount
type Status<'a> = Result of 'a | Error of string

let rec getHelp (a,k) = function
    | (a1,(n,p))::res     -> if a=a1 then (a1,(n-k,p))::res else (a1,(n,p))::(getHelp(a,k) res)
    | []                 -> []

let rec get (a,k) st = 
    match List.filter (fun (a1,(n,p)) -> a=a1 && k<=n) st with
    | [(a1,(n,p))] -> Result(k*p,getHelp(a,k) st)
    | [] -> failwithf "insufficient supply for %s" a

get ("a2",100) st
// val it : Status<int * (string * (int * int)) list> =
  Result (200, [("a1", (100, 10)); ("a2", (40, 20)); ("a3", (25, 40))])

// 3.4
let rec getAllAux (a,k) = function
    | (a1,(n1,p1))::tail        -> if a=a1 && k<=n1 then (a1,(n1-k,p1))::tail else (a1,(n1,p1))::getAllAux (a,k) tail
    | []                        -> []

let rec getAllHelp st = function
    | (a,k)::tail   -> getAllHelp  (getAllAux (a,k) st) tail
    | []            -> st

let rec getAllPrices st = function
    | (a,k)::tail    -> match List.tryFind (fun (a1,(n,p)) -> a1 = a && k<=n) st with
                            | Some (a1,(n,p))   -> k*p + getAllPrices st tail
                            | None              -> failwith "that order could not be handled"
    | []             -> 0

let os = [("a1", 30); ("a2", 40); ("a3", 3)]

let getAll os st = Result(getAllPrices st os, getAllHelp st os)

getAll os st
// Result (1220, [("a1", (70, 10)); ("a2", (10, 20)); ("a3", (22, 40))])

// Problem 4

//4.1
type T<'a> = 
    | L
    | A of 'a * T<'a>
    | B of 'a * T<'a> * T<'a>
    | C of 'a * T<'a> * T<'a> * T<'a>

C (3,B (2,L,L),A (2,A(0,L)), L)

// T is a sort of tree. 
// C is a node with a value 'a and 3 edges, B is a node with a value 'a and 2 edges and A is a  node with a value 'a and 1 edge.
// L is a leaf, sort of a way to stop the tree from "growing".
// Given the example above the type 'a is int.

// 4.2
let rec f1 t = 
    match t with
    | B(_, t1,t2) -> f1 t1 && f1 t2
    | L -> true
    | _ -> false
// val f1 : t:T<'a> -> bool
// checks if binary tree/only B and L is within t

let rec f2 t = 
    match t with
    | L -> L
    | A(i,t) -> A(i, f2 t)
    | B(i,t1,t2) -> B(i, f2 t2, f2 t1)
    | C(i,t1,t2,t3) -> C(i, f2 t3, f2 t2, f2 t1)
// val f2 : t:T<'a> -> T<'a>
// traverses the a given tree t

let rec f3 h = function
    | L -> L
    | A(i,t) -> A(h i, f3 h t)
    | B(i,t1,t2) -> B(h i, f3 h t1, f3 h t2)
    | C(i,t1,t2,t3) -> C(h i, f3 h t1, f3 h t2, f3 h t3)
// ('a -> 'b) -> T<'a> -> T<'b>
// Applies a function to every node's value in the tree

// Problem 5
type Title = string
type Document = Title * Element list
and Element =   Par of string 
              | Sec of Document

let s1 = ("Background", [Par "Bla"])
let s21 = ("Expressions", [Sec("Arithmetical Expressions", [Par "Bla"]); Sec("Boolean Expressions", [Par "Bla"])])
let s222 = ("Switch statements", [Par "Bla"])
let s223 = ("Repeat statements", [Par "Bla"])
let s22 = ("Statements",[Sec("Basics", [Par "Bla"]) ; Sec s222; Sec s223])
let s23 = ("Programs", [Par "Bla"])
let s2 = ("The Programming Language", [Sec s21; Sec s22; Sec s23])
let s3 = ("Tasks", [Sec("Frontend", [Par "Bla"]);Sec("Backend", [Par "Bla"])])

let doc = ("Compiler project", [Par "Bla"; Sec s1; Sec s2; Sec s3]);;

// 5.1

let rec noOfSecs = function
    | (x, Par (_)::es )    -> 0 + noOfSecs (x,es)
    | (x, (Sec es1)::es2)   -> 1 + noOfSecs es1 + noOfSecs (x,es2)
    | (_,[])                    -> 0

noOfSecs doc

// 5.2
// String.length("abc") = 3

let rec sizeOfDoc = function
    | (x, Par (y)::es)      -> String.length(y) + sizeOfDoc (x,es)
    | (x, (Sec (y,e))::es)   -> String.length(y) + sizeOfDoc (x,e) + sizeOfDoc (x,es)
    | (x,[])                -> 0 //String.length(x)

sizeOfDoc s21

// 5.3

let rec titlesInDoc = function
    | (x, Par _::es)      -> titlesInDoc (x,es)
    | (x, (Sec (y,e))::es)   -> y::titlesInDoc (x,e)@titlesInDoc (x,es)
    | (x,[])                -> []

titlesInDoc doc

// 5.4
type Prefix = int list;;
type ToC = (Prefix * Title) list


//let rec tocAux n = function
//    | []    -> []
//    | e::es -> tocHelp n e @ tocAux (n+1) es
//and tocHelp n = function
//    | (x, Par _::es)        -> (x,


// *** DECEMBER 17 ***


// Problem 4
type K<'a> = L | M of K<'a> * 'a * K<'a>

let v1 = L
let v2 = M (L,("a",[1;2]),L)
let v3 = M (v2,("b",[9;10]),M(L,("s",[5]),L))
let v4 = M (v2,("hej",[1;2;3]),v3) 

// Problem 5
type Term = | V of string
            | C of int 
            | F of string * Term list
;;
let ng1 = V "x"
let g1 = C 3
let g2 = F ("f0",[]) 
let g3 = F ("f1",[C 3;F("f0",[])]) 
let ng2 = F ("max",[V "x";C 3])

let rec isGroundAux = function
    | []        -> true
    | t::ts     -> isGround t && isGroundAux ts 
and isGround = function
    | V _           -> false
    | C _           -> true
    | F (_,t::ts)   -> isGround t && isGroundAux ts
    | F (_,[])      -> true



// 5.2
let rec toStringAux = function
    | []    -> ""
    | t::ts     -> "," + toString t  + toStringAux ts
and toString = function
    | V x           -> x
    | C n           -> string(n)
    | F (s,t::ts)   -> s + "(" + toString t + toStringAux ts + ")"
    | F (s, [])     -> s+"()"

let t6 = F("f3",[F("f2",[C 1; C 2]); F("f1",[V "x"]); F("f0",[])])

toString t6

// 5.3
let rec substAux x t = function
    | []            -> []
    | t1::ts         -> (subst x t t1)::(substAux x t ts)
and subst x t = function
    | V a           -> if x = V a then t else V a
    | C n           -> C n
    | F(s,t1::ts)   -> F(s,(subst x t t1)::(substAux x t ts))
    | F(s,[])       -> F (s,[])

subst (V "x") (C 117) t6

