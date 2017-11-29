open System.Security.Cryptography
open System.Web

// s164159 - Mathias Bo Jensen
// s165509 - Frederik Kirkegaard


type Outcome    = | S | F // S: for success and F: for failure
type Sample     = Outcome list
type ProbTree   = | Branch of string * float * ProbTree * ProbTree
                  | Leaf of string;;


// right tree of fig 1. (2/3)let exp = Branch(">2",0.67, Branch(">3",0.5, Leaf "A", Leaf "B"), Branch(">3",0.5, Leaf "C", Leaf "D"));;

// 1
// Frederik du skal nok lige tjekke den her, er lidt usikker på Leaf tilfældet.

let rec probOk t = 
    match t with
    | Branch(_,p,tl,tr) -> (0.0 < p) && (p <= 1.0) && probOk tl && probOk tr
    | Leaf s -> true;;

probOk exp

// 2

let rec isSample = function
    | ([], Leaf s) -> true
    | (_, Leaf s) -> false
    | ([], _) -> false
    | (x::xs,Branch(_,_,tl,tr)) -> if x = S then isSample(xs,tl) else isSample(xs,tr);;
   
isSample ([F;S],exp)

// 3

type Description =  ((Outcome * string) list) * float * string
// skod List.rev
let rec descriptionOfAux = function
    | (_,Leaf s1, (ys,p2,s2)) -> ((List.rev ys),p2,s2+s1) 
    | (x::xs,Branch(ds,p1,tl,tr),(ys,p2,s)) when x = F -> descriptionOfAux(xs,tr,((x,ds)::ys,(1.0-p1)*p2,s))  
    | (x::xs,Branch(ds,p1,tl,tr),(ys,p2,s)) -> descriptionOfAux(xs,tl,((x,ds)::ys,(p1)*p2,s))


let descriptionOf os t = if isSample(os,t) then descriptionOfAux(os,t,([], 1.0, "")) else failwith "Sample not correct"

descriptionOf [S;F] exp

// 4

// compute depth of tree

let rec depth = function
    | Leaf _    -> 0
    | Branch (_,_,tl,tr) -> 1+ max (depth tl) (depth tr)

// Combinations

let rec allDescriptionsAux = function
    | (Leaf s1,(ys,p,s2)) -> set [(List.rev ys,p,s1)]
    | (Branch(ds,p1,tl,tr), (ys,p2,s)) -> Set.union (allDescriptionsAux(tl,((S,ds)::ys,p1*p2,s))) (allDescriptionsAux(tr,((F,ds)::ys,(1.0-p1)*p2,s)));;

let allDescriptions t = allDescriptionsAux (t,([],1.0,""))

allDescriptions exp

let rec probabilityOfAux = function
    | (Leaf s1,(ys,p,s2),pred) -> if pred s1 then p else 0.0
    | (Branch(ds,p1,tl,tr), (ys,p2,s), pred) -> probabilityOfAux(tl,((S,ds)::ys,p1*p2,s),pred) + probabilityOfAux(tr,((F,ds)::ys,(1.0-p1)*p2,s),pred);;

let probabilityOf t pred = probabilityOfAux (t,([],1.0,""),pred)

probabilityOf (exp) (fun s -> s = "C" || s = "B")

