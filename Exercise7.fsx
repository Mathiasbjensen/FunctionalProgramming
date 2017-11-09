type FileSys = Element list 
    and Element =
    | File of string * string
    | Dir of string * FileSys

// ListOfNames
let d1 = 
    Dir("d1",[File("a1","java"); 
        Dir("d2", [File("a2","fsx"); 
            Dir("d3", [File("a3","fs")])]); 
        File("a4","fsx"); 
        Dir("d3", [File("a5","pdf")])]);;


let rec namesFileSys = function 
    | [] -> [] 
    | e::es -> (namesElement e) @ (namesFileSys es) 
and namesElement = function 
    | File (s,s1) -> [s+"."+s1] 
    | Dir(s,fs) -> s :: (namesFileSys fs)

namesElement d1

// search

let rec searchFileSys ext = function
    | [] -> Set.empty
    | e::es -> Set.union (searchElement ext e) (searchFileSys ext es)
and searchElement ext = function
    | File (s,s1) -> if s1 = ext then Set.singleton s else Set.empty
    | Dir (s,fs) -> searchFileSys ext fs

searchElement "fsx" d1


// longNames

let rec longNamesFileSys = function
    | [] -> Set.empty
    | e::es -> Set.union (longNamesElement e) (longNamesFileSys es) 
and longNamesElement = function
    | File (s,s1) ->  set [s+"."+s1]
    | Dir (s,fs) -> Set.map ((+) (s+"\\")) (longNamesFileSys fs)

longNamesElement d1
