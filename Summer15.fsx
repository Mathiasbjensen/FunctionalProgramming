// Summer 2015

// Problem 1

// 1.1
let rec repeat s n =
    match n with
    | 0 -> ""
    | n -> s + repeat s (n-1)

repeat "ab" 4

// 1.2
let rec repeatn s1 s2 n =
    match n with
    | 0 -> ""
    | n -> s1 + "\n" + repeatn s2 s1 (n-1)

repeatn "ab" "cd" 4

// 1.3 
let repeatXO m n = repeatn (repeat "XO" m) (repeat "OX" m) n 
repeatXO 4 5

// 1.4.1 tail recursuve
let rec repeatA acc s n =
    match n with
    | 0 -> acc 
    | n -> repeatA (s + acc) s (n-1)

repeatA "" "ab" 4

// 1.4.2 continuation based
let rec repeatC k s n = 
    match n with
    | 0 -> k ""
    | n -> repeatC (fun v -> k(s+v)) s (n-1)

repeatC id "ab" 

//    
