#if INTERACTIVE
#load "../Common.fsx"
#else 
module AoC
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let test1  = load folder "test1.txt"
//let test2  = load folder "test2.txt"
    
let p1 a = 0
let p2 a = 0

//Test.equal "Test1" 142 (p1 test1)

let part1 = p1 input
let part2 = p2 input

printfn "Part1: %i" part1
printfn "Part2: %i" part2
