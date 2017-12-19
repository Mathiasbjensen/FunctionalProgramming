open System.Security.Cryptography

// December 15

// Problem 1

type Appliance = string
type Usage = Appliance * int

let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ad1; ad2; ad3; ad1; ad2];;

// 1.1
let inv ats = List.forall (fun (a,t) -> t>=0) ats

inv ats

// 1.2
let durationOf a ats = 
    List.fold (fun acc (a1,t) -> if a1 = a then acc + t else acc) 0 ats

durationOf "coffee machine" ats

// 1.3
let wf ats = inv ats && List.forall (fun (a,_) -> (durationOf a ats) <=24) ats

wf ats

// 1.4
let rec delete(a,ats) = 
    match ats with
    | [] -> []
    | (a1,ats1)::tail when a1 = a -> delete(a,tail)
    | (a1,ats1)::tail -> (a1,ats1)::delete(a,tail)

delete("coffee machine",ats)

// 1.5
type Price = int
type Tariff = Map<Appliance, Price>

let trf = Map.ofList [("coffee machine", 100);("dishwasher", 200);("washing machine", 300)]

let isDefined ats trf = List.forall (fun (a,_) -> Map.containsKey a trf) ats
isDefined ats trf

// 1.6

let rec priceOf ats trf = 
   match ats with
   | [] -> 0
   | (a,t)::tail -> match Map.tryFind a trf with
                    | Some c -> t*c + priceOf tail trf
                    | None -> failwith "shit"

priceOf ats trf
        


// Problem 2

let rec g1 p = function
    | x::xs when p x -> x :: g1 p xs
    | _ -> [];;

let rec g2 f h n x =
    match n with
    | _ when n<0 -> failwith "negative n is not allowed"
    | 0 -> x
    | n -> g2 h f (n-1) (f x);;

// 2.1

// 2.2  
let p1 x = x>1
let somelist = [1;2;3;4;5]
// Tail recursive version
let rec g1A acc p xs = 
    match xs with
    | x::xs when p x    -> g1A(x::acc) p xs
    | x::xs             -> g1A acc p xs
    | []                -> List.rev acc

g1A [] p1 somelist

//  Continunation-based
let rec g1C p xs k =
    match xs with
    | [] -> k []
    | x::xs when p x    -> g1C p xs (fun v-> k(x::v))
    | x::xs             -> g1C p xs k

g1C p1 somelist id

// 2.3
let f1 m n k = seq { for x in [0..m] do
                        for y in [0..n] do
                            if x+y < k then
                                yield (x,y) };;


let f2 f p sq = seq { for x in sq do
                        if p x then
                            yield f x };;

let f3 g sq = seq { for s in sq do
                        yield! g s };;


// 2.4
List.ofSeq (f1 2 2 3) // = [(0, 0); (0, 1); (0, 2); (1, 0); (1, 1); (2, 0)]

// 2.5 

let f2Alt f p sq = 
    let sift = Seq.filter (fun n -> p n) sq
    Seq.map (fun s -> f s) sift

// A list and another function to test f2
let newseq = Seq.init 5 (fun i -> 1+i)
let ff x = x*2

f2Alt ff p1 newseq
f2 ff p1 newseq


