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

