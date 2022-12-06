#if INTERACTIVE
#load "../Common.fsx"
#else
module Day3
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

let input  = load folder "input.txt" |> Array.map id

let score (x:char) = if System.Char.IsLower x then int x - 96 else int x - 38

let part1 = 
    input
    |> Array.map
        (fun (str) -> 
            let l = str.Length / 2
            let a = str.Substring(0, l)
            let b = str.Substring(l)
            let sa = a.ToCharArray() |> Set.ofArray
            let sb = b.ToCharArray() |> Set.ofArray
            let s = Set.intersect sa sb
            Set.fold (fun _ x -> score x) 0 s
        )
    |> Array.sum

[|0..10|] |> Array.chunkBySize 3

let part2 = 
    input
    |> Array.chunkBySize 3
    |> Array.map
        (fun (group) -> 
            let a = group.[0].ToCharArray() |> Set.ofArray
            let b = group.[1].ToCharArray() |> Set.ofArray
            let c = group.[2].ToCharArray() |> Set.ofArray
            let r = Set.intersectMany [a;b;c]
            Set.fold (fun _ x -> score x) 0 r
        )
    |> Array.sum


//
printfn "%i" part1
printfn "%i" part2

