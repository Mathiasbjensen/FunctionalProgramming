// Mathias Bo Jensen - s164159
// Frederik Kirkegaard - s165509

type Name = string
type Flow = int
type River = R of Name * Flow * Tributaries
and Tributaries = River list
 
// 1
let riv3 = R ("R3", 8, [])
let riv = R ("R", 10, [R ("R1", 5, []) ; R ("R2", 15, [R ("R4", 2, [])]) ;
            riv3])
 
// 2
let rec containsAux n = function
    | [] -> false
    | t::tail -> contains n t || containsAux n tail
and contains n = function
    | R (n1,_,t) -> n1=n || containsAux n t
 
// 3
let rec allNamesAux = function
    | [] -> []
    | t::tail -> (allNames t) @ (allNamesAux tail)
and allNames = function
    | R (n1,_,t) -> n1 :: (allNamesAux t)
 
// 4
let rec totalFlowAux = function
    | [] -> 0
    | t::tail -> (totalFlow t) + (totalFlowAux tail)
and totalFlow = function
    | R (n1,f,t) -> f + (totalFlowAux t)

 
// 5
let rec helpMainSource = function
    |(n1,f)::(n2,f2)::t -> if f > f2 then helpMainSource ((n1,f)::t) else helpMainSource ((n2,f2)::t)
    |[(n,f)] -> (n,f)
 
let rec mainSourceAux = function
    | [] -> [("",0)]
    | t::tail -> (mainSource t) :: (mainSourceAux tail)
and mainSource = function
    | R (n1,f,t) -> helpMainSource((n1,f) :: (mainSourceAux t));;
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

// 7
(* An issue with the way we have defined our rivers is, that we can essentially end up in an infinite loop. 
An example would be if we have a river called 'R1', which has a tributary 'R2' and 'R2' has a tributary 'R3' and 'R3' has 'R1' as a tributary.
This way we end up having an infinite loop when describing the river R1 with its tributaries. *)

// ### TESTS ###
 
// 2 test
let testContains1 = contains "R1" riv = true
let testContains2 = contains "R5" riv = false
 
// 3 tests
let testAllNames = allNames riv = ["R";"R1";"R2";"R4";"R3"]
// 4 tests
let testTotalFlow1 = totalFlow riv = 40
 
// 5 tests
let testMainSource = mainSource riv = ("R2",15)

// 6 tests
let testTryInsert1 = tryInsert "R1" riv3 riv = Some (R ("R1", 5, [R ("R3", 8, [])]))
let testTryInsert2 = tryInsert "R5" riv3 riv = None

