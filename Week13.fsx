// 08/12-2017

// 1
// Use list.fold to get the sum of all integers bigger than k in the list.
let sumGt k xs = List.fold (fun acc x -> if x > k then acc + x else acc ) 0 xs

sumGt 4 [1;5;2;7;4;8]

// Exercise 2 

type 'a tree =  | Lf
                | Br of 'a * 'a tree * 'a tree;;

// f: (int * 'a tree) -> 'a tree

// if it reaches a leaf it returns a leaf, otherwise it goes recursively through the 
// branches left and right sub branch n times. When n = 0 it replaces
// the branch node with a leaf - cuts the tree off to an n depth

// g: 'a tree -> (a -> bool) -> 'a tree

// a filter function for the predicate p. replaces the nodes that do not fulfull
// the predicate with a leaf.

// h: ('a -> 'b) 'a tree -> 'b tree
// k is a function that takes the value a in the node. So it applies the function k
// to the value a in all branches. (a map function).

// Exercise 2

type Container =    | Tank of int * int * int // (length, width, height)
                    | Ball of int // radius
                    | Cylinder of int * int

// 2.1
let Tank1 = Tank (1,2,3)
let Ball1 = Ball 5

// 2.2
let isWF = function
    | Tank (a,b,c)  -> a > 0 && b > 0 && c > 0
    | Ball r        -> r > 0

isWF (Ball -2)

// 2.3

//let volume = function
//    | Tank (a,b,c)  -> float(a*b*c)
//    | Ball r -> 4.0/3.0*System.Math.PI*float(r*r*r)

//volume (Ball 2)



// 2.4
let volume = function
    | Tank (a,b,c)      -> float(a*b*c)
    | Ball r            -> 4.0/3.0*3.14*float(r*r*r)
    | Cylinder (r1,h)   -> 3.14*float(r1*r1)*float(h)

volume (Cylinder (2,3))

// 2.5
type Name = string
type Contents = string
type Storage = Map<Name, Contents*Container> ;;

//let Storage1 = Storage (Map.ofList [("tank1",("oil",Tank (1,2,3)));("ball1",("water", Ball 5))])

let test = (Map.add "tank1" ("oil",Tank (1,2,3)) Map.empty)

// 2.6

let find n stg = 
    match Map.tryFind n stg with
    | None -> failwith "No container found with that name"
    | Some (cnt, cont) -> (cnt, (volume cont))

find "tank1" test

// ctrl + k + c for comment   -  ctrl + k + u for uncomment.

// Exercise 3

type Appliance = string
type Usage = Appliance * int

let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ad1; ad2; ad3; ad1; ad2]

// 3.1

let inv ats = List.forall (fun (a,t) -> t > 0) ats

inv ats

// 3.2

let durationOf a ats = List.fold (fun acc (a1,t) -> if a1 = a  then acc + t else acc) 0 ats

durationOf "washing machine" ats

// 3.3

let wf ats = inv ats && List.forall (fun (x,_) -> (durationOf x ats) <= 24) ats 
wf ats

// 3.4

let rec delete (a,ats) =
    match ats with
    | [] -> []
    | (a1,t)::axs when a1 <> a -> (a1,t)::delete(a,axs)
    | (a1,t)::axs -> delete(a,axs)


delete ("coffee machine", ats)

 
