
// 13/10
//Exercises 6

// 5.1
type name = string
type number = string
type sex = string
type birthday = int
type interest = Set<string>
type qualities = number*sex*birthday*interest
type Person = name * qualities

let paul : Person = ("Paul" , ("29921039", "Male", 1996, set ["Sport"; "Movies"; "Drinking"]))
let sisse : Person = ("Sisse", ("10392049", "Female", 1990, set ["Sport";"Movies";"Shopping"]))
let skod : Person = ("Skod", ("20394010", "Male", 1999, set ["Drinking"; "Movies";"Laughing"]))

let archive = Map.ofList [paul ; sisse ; skod]

let findMatch p1 archive1 =
    match p1 with
    | (_, (_, s1, b1, i1)) -> Map.filter (fun _ (_, s, b, i) -> s <> s1 && b > b1-10 && b < b1+10 && Set.count(Set.intersect i1 i) > 0) archive1

let test = findMatch sisse archive


// 5.7
set [ set [1;2] ; set [2;1]]


Set.ofSeq [1..3]

let rec allSubsets n k =
    let list = Set.ofSeq [1..n]
    match k with
    | 

        

