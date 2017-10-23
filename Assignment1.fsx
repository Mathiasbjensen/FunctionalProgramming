// s164159 Mathias Bo Jensen
// s165509 Frederik Kirkegaard
// Assignment 1

type Member = {
    Name : string;
    No : string;
    Yb : int;
    Ths : string list
    } 


// Register:
// Member list
type Register = (Member list)

// Declaring a register (Member list)
let reg = [
    {
        Name = "Hans";
        No = "12341234";
        Yb = 1990;
        Ths = ["soccer" ; "beer"]
        } ;
    {
        Name = "Simon";
        No = "87654321";
        Yb = 1994;
        Ths = ["food" ; "climbing"]
        } ;
    {
        Name = "Robert";
        No = "21436587";
        Yb = 1984;
        Ths = ["jazz" ; "soccer"]
        } ;    
    ]
    

// Finding people interested in a certain arrangement
// Member list --> 'a --> bool --> (string * string) list
let rec extractInterested p r =
    match r with
    | [] -> []
    | head::tail when p(head) -> (head.Name,head.No)::extractInterested p tail
    | _::tail -> extractInterested p tail


// functions for checking if a member would be interested in a certain arrangement:
// Member --> bool
let p1 (m:Member) =
    (m.Yb > 1982) && (List.contains "jazz" m.Ths && List.contains "soccer" m.Ths)

// Member --> bool
let p2 (m:Member) =
    (m.Yb > 1982) && (List.contains "jazz" m.Ths || List.contains "soccer" m.Ths)
    
// Tests

// Tests for p1 function
let testp1True = p1 {Name = "Michael"; No = "78945612" ; Yb = 1990 ; Ths = ["soccer";"jazz"]} = true
let testp1False = p1 {Name = "Tina"; No = "78945612" ; Yb = 1950 ; Ths = ["soccer";"jazz"]} = false
let testp1False2 = p1 {Name = "Anne"; No = "78945612" ; Yb = 1990 ; Ths = ["art";"jazz"]} = false

// Tests for p2 function
let testp2True = p2 {Name = "Mathias"; No = "78945612" ; Yb = 1990 ; Ths = ["soccer";"beer"]} = true
let testp2True2 = p2 {Name = "Mathias"; No = "78945612" ; Yb = 1990 ; Ths = ["soccer";"jazz"]} = true
let testp2False = p2 {Name = "Frederik"; No = "78945612" ; Yb = 1990 ; Ths = ["sports";"beer"]} = false
let testp2False2 = p2 {Name = "Soren"; No = "78945612" ; Yb = 1966 ; Ths = ["sports";"beer"]} = false

// Test for extractInterested
let testExtractP1 = extractInterested p1 reg = [("Robert","21436587")]
let testExtractP2 = extractInterested p2 reg = [("Hans","12341234");("Robert","21436587")]

