type CourseNo   = int
type Title      = string
type ECTS       = int
type CourseDesc = Title * ECTS 

type CourseBase = Map<CourseNo, CourseDesc>

type Mandatory   = Set<CourseNo>
type Optional    = Set<CourseNo> 
type CourseGroup = Mandatory * Optional

type BasicNaturalScience      = CourseGroup
type TechnologicalCore        = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective                 = CourseNo -> bool

type FlagModel  = BasicNaturalScience * TechnologicalCore 
                   * ProjectProfessionalSkill * Elective                 
type CoursePlan = Set<CourseNo>   

// 1)

// let isValidCourseDesc (desc:CourseDesc) =
let isValidCourseDesc desc =
    match desc with
    | (_,E) -> if E % 5 = 0 && E > 0 then true else false
    
isValidCourseDesc ("skod",3)
isValidCourseDesc ("skod2",10)
isValidCourseDesc ("skod3",-15)

// 2
let isValidCourseBase (cb:CourseBase) = Map.forall (fun _ (desc:CourseDesc) -> isValidCourseDesc desc  = true) cb

let test = [(100,("test",10)); (101,("test2",12))]
let test2 = Map.ofList test

isValidCourseBase test2

// 3

let disjoint = function
    | (rs, ys) -> if Set.intersect rs ys = set [] then true else false

let testdis = disjoint((set [1;2;3]), (set [4;5;6]))

// 4
let rec sumECTS = function
    | (cs,_) when Set.count (cs) = 0 -> 0
    | (cs,cb) -> 
        let elem = (Set.maxElement(cs))
        match Map.tryFind elem cb with
        | Some(cs2,E) -> E+sumECTS(Set.remove elem cs,cb)
        | None -> sumECTS(Set.remove elem cs,cb)


let testnumbers = set [100;101]
let testsum = sumECTS(testnumbers,test2)

