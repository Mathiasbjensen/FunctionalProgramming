// Mathias Bo Jensen - s164159
// Frederik Kirkegaard - s165509

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

// 1
let isValidCourseDesc desc =
    match desc with
    | (_,E) -> if E % 5 = 0 && E > 0 then true else false
    

// 2
let isValidCourseBase (cb:CourseBase) = Map.forall (fun _ (desc:CourseDesc) -> isValidCourseDesc desc  = true) cb

// 3

let disjoint = function
    | (rs, ys) -> if Set.intersect rs ys = set [] then true else false

// 4
let rec sumECTS = function
    | (cs,_) when Set.count (cs) = 0 -> 0
    | (cs,cb) -> 
        let elem = (Set.maxElement(cs))
        match Map.tryFind elem cb with
        | Some(cs2,E) -> E+sumECTS(Set.remove elem cs,cb)
        | None -> sumECTS(Set.remove elem cs,cb)


// 5
let isValidCourseGroup cg cb =
    match cg with
    |(man,opt) -> 
        let sumMan = sumECTS(man,cb)
        let sumOpt = sumECTS(opt,cb)
        (sumMan <= 45) && disjoint(man,opt) && (sumMan+sumOpt) >= 45 && not ((sumMan = 45) && (opt <> Set.empty))


// 6
// Takes 2 sets and unifies them to 1 set.
let unionSets = fun (rs,ys) -> Set.union rs ys
// Testing if all 3 CourseGroups are valid.
let threeCourseValid = fun bns tc pps cb -> (isValidCourseGroup bns cb && isValidCourseGroup tc cb && isValidCourseGroup pps cb)

let isValid = fun ((bns,tc,pps,ep):FlagModel,cb:CourseBase) -> 
    let bnsUnion = unionSets bns
    let tcUnion = unionSets tc
    let ppsUnion = unionSets pps
    (disjoint(bnsUnion,tcUnion) && disjoint(bnsUnion,ppsUnion) && disjoint(tcUnion,ppsUnion) && 
        threeCourseValid bns tc pps cb && Set.forall ep (unionSets (unionSets (bnsUnion,tcUnion),ppsUnion)))


// 7
let checkPlan cp fm cb =
    match fm with
    |(bns,tc,pps,ep) ->
    let bnsInCp = Set.intersect (unionSets bns) cp
    let tcInCp = Set.intersect (unionSets tc) cp
    let ppsInCp = Set.intersect (unionSets pps) cp
    ((sumECTS(cp,cb) = 180) && (sumECTS(bnsInCp,cb) >= 45) && (sumECTS(tcInCp,cb) >= 45) && (sumECTS(ppsInCp,cb) >= 45) && 
        (isValid((bns,tc,pps,ep),cb) && (Set.forall ep cp)))


// Test declarations
let test = [(100,("test",30)); (101,("test2",30)); (102,("CsTest1",15)); (200,("test3", 30));(201,("test4", 30)); (202,("CsTest2",15)); (300,("test5", 30));(301,("test6",30)) ; (302,("CsTest3",15)); (400,("test7",15)) ; (401,("test8",15)) ; (402,("test9",15))]
let test2 = Map.ofList test

let cpTest1 = set [100 ; 102 ; 200 ; 202 ; 300 ; 302 ; 400 ; 401 ; 402]
let cpTest2 = set [100 ; 102 ; 200 ; 202 ; 300 ; 302 ; 400 ; 401]
let cpTest3 = set [100 ; 102 ; 200 ; 202 ; 300 ; 302 ; 400 ; 401 ; 402 ; 101]

let testnumbers1 = set [100;101]
let testnumbers2 = set [400;401;402]
let testnumbers3 = set [400;401;402;999]

let testOptional1 = set [100;101]
let testMandatory1 = set [200;201]
let testOptional2 = set [100;102]
let testMandatory2 = set [200;202]
let testOptional3 = set [100]
let testMandatory3 = set [200]

let testBns = (set [102],set [100])
let testTc = (set [202],set [200])
let testPps = (set [302],set [300])

let flagModeltest1 = (testBns,testTc,testPps,fun e -> e > 99)
let flagModeltest2 = (testBns,testTc,testPps,fun e -> e < 99)

// ### TESTS ###

// Test for 1.
let validCourseDescTest1 = isValidCourseDesc ("Mat1",3) = false
let validCourseDescTest2 = isValidCourseDesc ("Mat2",10) = true
let validCourseDescTest3 = isValidCourseDesc ("Mat3",-15) = false

// Test for 2.
let validCourseBaseTest = isValidCourseBase test2 = true

// Test for 3.
let testdis1 = disjoint((set [1;2;3]), (set [4;5;6])) = true
let testdis2 = disjoint((set [1;2;3]), (set [4;5;6;1])) = false

// Test for 4.
let testsum1 = sumECTS(testnumbers1,test2) = 60
let testsum2 = sumECTS(testnumbers2,test2) = 45
let testsum3 = sumECTS(testnumbers3,test2) = 45

// Test for 5.
let vcgTest1 = isValidCourseGroup (testOptional1,testMandatory1) test2 = false
let vcgTest2 = isValidCourseGroup (testOptional2,testMandatory2) test2 = false
let vcgTest3 = isValidCourseGroup (testOptional3,testMandatory3) test2 = true

// Test for 6.
let testIsValid1 = isValid(flagModeltest1,test2) = true
let testIsValid2 = isValid(flagModeltest2,test2) = false

// Test for 7.
let checkPlantest1 = checkPlan cpTest1 flagModeltest1 test2 = true
let checkPlantest2 = checkPlan cpTest2 flagModeltest1 test2 = false
let checkPlantest3 = checkPlan cpTest3 flagModeltest1 test2 = false
