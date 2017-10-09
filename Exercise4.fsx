//Exercises4

type Person = {name: string; number: int; sex: string; birthday: int; interest : string list}

let paul = {name = "Paul"; number = 29921039; sex = "Male"; birthday = 1995; interest = ["Sport"; "Movies"; "Drinking"]};;
let sisse = {name = "Sisse"; number = 10392049; sex = "Female"; birthday = 1990; interest = ["Sport";"Movies";"Shopping"]};;
let skod = {name = "Skod"; number = 20394010; sex = "Male"; birthday = 2000; interest = ["Drinking"; "Movies";"Laughing"]};;

let archive = [paul; sisse ; skod];;

let rec findMatch p1 = function
       | [] -> []
       | head::tail when head.sex <> p1.sex && (head.birthday >= p1.birthday - 10 || head.birthday <= p1.birthday + 10) -> head::findMatch p1 tail
       | head::tail -> findMatch p1 tail;;

findMatch sisse archive;;