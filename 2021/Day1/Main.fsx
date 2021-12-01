#if INTERACTIVE
#load "../Common.fsx"
#endif

//START: 08:11 - 08:16 - 8:23 - inklusive påklädning av barn

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt" |> Array.map (Parse.int32)

let countDec arr = 
    arr
    |> Array.fold 
        (fun (p,s) x -> (Some x, match p with |Some p when x > p -> s+1 |_ -> s )) 
        (None, 0)
    |> snd

let part1 = countDec input
let part2 = 
    input
    |> Array.windowed 3
    |> Array.map Array.sum
    |> countDec

[<EntryPoint>]
let main argv =
    printfn "Part1: %i" part1
    printfn "Part2: %i" part2
    0
