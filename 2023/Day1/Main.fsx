#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day1
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let test1  = load folder "test1.txt"
let test2  = load folder "test2.txt"
    
let indexes str cmp =      
    let rec next (str:string) (substr:string) (loc:int) (indexes:int list) = 
        let nxt = str.IndexOf(substr, loc)
        if nxt = -1 then indexes
        else next str cmp (nxt+1) (nxt::indexes)
    next str cmp 0 []

///Finds first and last <value> of input.
/// Values are defined with list nums. Which defines a string and its corresponding value - ex: "one",1
let getNums (nums) (inp:string) = 
    let n = 
        nums
        |> List.collect (fun (str,value) -> indexes inp str |> List.map (fun i -> i, value))
        |> List.sortBy fst
        |> List.map snd
    Parse.int32 (sprintf "%i%i" (n |> List.head) (n |> List.last))

let solve nums input = input |> Array.map (getNums nums) |> Array.sum

let nums1 = [0..9] |> List.map (fun x -> sprintf "%i" x, x)
let nums2 = ["one",1;"two",2;"three",3;"four",4;"five",5;"six",6;"seven",7;"eight",8;"nine",9] 

let p1 = solve nums1 
let p2 = solve (List.append nums1 nums2)

Test.equal "Test1" 142 (p2 test1)

let part1 = p1 input
let part2 = p2 input


printfn "Hejsan allaihopa"

//
printfn "Part1: %i" part1
printfn "Part2: %i" part2

printfn "Hejsan"