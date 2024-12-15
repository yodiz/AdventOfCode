#if INTERACTIVE
#load "../Common.fsx"
#else 
module AoC
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let test1  = load folder "test1.txt"
//let test2  = load folder "test2.txt"
    

let rec findSum wantedSum remaining sum : bool = 
    match remaining with
    |[] -> if wantedSum = sum then true else false
    |n::rest -> 
        findSum wantedSum rest (sum+n)
        || findSum wantedSum rest (sum*n)


let rec findSum2 wantedSum remaining sum : bool = 
    match remaining with
    |[] -> if wantedSum = sum then true else false
    |n::rest -> 
        findSum2 wantedSum rest (sum+n)
        || findSum2 wantedSum rest (sum*n)
        || findSum2 wantedSum rest (sprintf "%i%i" sum n |> Parse.int64)


//292: 11 6 16 20
let parse (p) = 
    let (sum, operators) = p |> Text.split2 ": "
    Parse.int64 sum, operators |> Text.split " " |> Array.map Parse.int64



//parse "292: 11 6 16 20"
let p1 a = 
    a
    |> Array.map parse
    |> Array.filter 
        (fun (s,n) -> 
            let n2 = (n |> Array.toList)
            findSum s n2.Tail n2.Head        
        )
    |> Array.map fst
    |> Array.sum
    
let p2 a = 
    a
    |> Array.map parse
    |> Array.filter 
        (fun (s,n) -> 
            let n2 = (n |> Array.toList)
            findSum2 s n2.Tail n2.Head        
        )
    |> Array.map fst
    |> Array.sum

Test.equal "Test1" 3749L (p1 test1)
Test.equal "Test2" 11387L (p2 test1)

let part1 = p1 input
let part2 = p2 input

printfn "Part1: %i" part1
printfn "Part2: %i" part2
