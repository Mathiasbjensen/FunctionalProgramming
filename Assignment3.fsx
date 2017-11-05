open System.Security.Cryptography

type Name = string
type Flow = int
type River = R of Name * Flow * Tributaries
and Tributaries = River list

let riv = R ("R", 10, [R ("R1", 5, []) ; R ("R2", 15, [R ("R4", 2, [])]) ; 
            R ("R3", 8, [])])

let riv3 = R ("R3", 8, [])

let rec inTributaries n t =
    match t with
    | R (r1,_,r2) when r1 = n -> true
    | R (r1,_,[]) -> if r1=n then true else false


let rec contains n r =
    match r with
    | R (r1, _, _) when r1=n -> true
    | R (_, _, []) -> false
    | R (_, _, r2::tail) -> if r1 = n then true else contains n r2

contains "R1" riv


(* 
| (x,_,[]) -> if  x ->