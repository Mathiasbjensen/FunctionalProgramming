// 11.1

let nat = Seq.initInfinite (fun i -> i)
let odd = Seq.filter (fun i -> i % 2 <> 0)  nat
Seq.nth 4 odd

// 11.2

let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n-1)


let fact = Seq.initInfinite factorial

Seq.nth 5 fact

// 11.3


let rec factseq = function
    | 0 | 1 -> seq {yield 1}
    | n -> let res = factseq (n-1) 
           Seq.append  res (Seq.singleton((n-1) * Seq.last res))

Seq.nth 5 (factseq 100)

// 11.9
let rec enum sq = 
    seq { let p = Seq.head sq
          yield p*(-1)
          //if p <> 0 then yield p
          yield p
          yield! enum (Seq.tail sq) }


Seq.toList(Seq.take 10 (enum nat))

// 11.10

let cartesian sqx sqy =
    seq { for x in sqx do 
            for y in sqy do 
                yield (x,y) }

Seq.toList (cartesian (seq [1;2;3;4]) (seq [5;6;7;8]))

// Summer exam 2014 Problem 1 

// 4
let sq = Seq.initInfinite (fun i -> 3*i)
Seq.toList(Seq.take 10 sq)

let k j = seq {for i in sq do
                yield (i,i-j) }

// Fall exam 2014 Problem 2


// 1 
let multTable n = 
        let nat1 = Seq.initInfinite (fun i -> i+1)
        Seq.take 10 (Seq.map (fun i -> i*3) nat1)

Seq.toList (multTable 3)

// 2 

let tableOf m n f =
        let seqNat = Seq.initInfinite (fun i -> i+1)
        seq { for x in (Seq.take m seqNat) do
                for y in (Seq.take n seqNat) do
                    yield(x,y,f x y) }

Seq.toList (tableOf 3 4 (+))

// 3

