#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day1
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"

let elvesCal =
    input
    |> Array.fold 
        (fun (l,s) x -> if x = "" then [],l::s else (AoC.Parse.int32 x::l,s))
        ([], [])
    |> snd
    |> List.map (fun x -> List.sum x)
    |> List.sortDescending

let part1 = elvesCal |> List.pick Some
let part2 = elvesCal |> List.take 3 |> List.sum

//
printfn "Part1: %i" part1
printfn "Part2: %i" part2

