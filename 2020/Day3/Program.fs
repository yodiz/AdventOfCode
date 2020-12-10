#if INTERACTIVE 
#load "../Common.fsx"
#endif
open AoC

let parseLine (inp:string) = 
    inp.ToCharArray() |> Array.map (function |'.' -> false |'#' -> true)

let rec walkCoord (inp:bool array array) (x,y) (dx,dy) (trees) = 
    if y >= inp.Length then
        trees
    else
        let width = inp.[0].Length
        let isTree = inp.[y].[x%width]
        Trace.output (sprintf "x:%i,y:%i" x y)
        walkCoord inp (x+dx,y+dy) (dx,dy) (if isTree then trees+1 else trees)
        
let test1 = 
    let inp = load "Day3/test1.txt" |> Array.map parseLine 
    Trace.withTrace (fun () -> 
        walkCoord inp (0,0) (3,1) 0
        |> Test.equal "Test1" 7
    )

let part1 = 
    let inp = load "Day3/input.txt" |> Array.map parseLine 
    walkCoord inp (0,0) (3,1) 0

let part2 = 
    let inp = load "Day3/input.txt" |> Array.map parseLine 
    let walk x y = walkCoord inp (0,0) (x,y) 0   
    [
        walk 1 1   
        walk 3 1   
        walk 5 1   
        walk 7 1   
        walk 1 2   
    ]
    |> List.map int64 |> List.reduce ((*))
 
printfn "part1 %i" part1
printfn "part2 %i" part2
