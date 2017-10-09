// Exercise5

// 3.5
type Solution = 
    | OneRoot of float
    | TwoRoots of float*float
    | NoRoots

let getRoot a b d symb = 
    symb -b (sqrt d) / (2.0*a)

let Solve a b c =
    let d = b*b-4.0*a*c
    match d with
    | 0.0 -> OneRoot (getRoot a b d (+))
    | d when d > 0.0 -> TwoRoots (getRoot a b d (+) , getRoot a b d (-))
    | d when d < 0.0 -> NoRoots

Solve 2.0 1.0 8.0

// 5.2

let reverseList xs = List.fold (fun rs x -> x::rs) [] xs

let revrev xs = List.fold (fun rs x -> reverseList x::rs) [] xs

let skod = [[1;2;3;4;5];[6;7;8]]
revrev skod


// 5.3
let skod2 = [1;2;3;4]

let p x = x > 0

// x er "state" altså den tidligere værdi (første gang vil det være 0)
// y er den værdi der skal tjkekkes for.
let sum(p,xs) = List.fold (fun x y -> if p y then x+y else x) 0 xs

sum(p,skod2)

// 5.5

let areNB m c1 c2 = List.forall (fun x -> (x<>(c1,c2) || x<>(c2,c1))) m

let maps = [("a","b");("c","d");("e","b")]

areNB maps "a" "b"

