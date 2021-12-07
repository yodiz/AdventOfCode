#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day2
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

let parseLine (str:string) = 
    match str.Split([|' '|]) with
    |[|command; Int32 num |] -> command, num
    |_ -> failwithf ""

let input  = load folder "input.txt" |> Array.map parseLine

type State = { Depth : int; Forward : int; Aim : int }

let part1 = 
    input
    |> Array.fold 
        (fun s (cmd,n) -> 
            match cmd with
            |"forward" -> { s with Forward = s.Forward + n }
            |"down" -> { s with Depth = s.Depth + n }
            |"up" -> { s with Depth = s.Depth - n }
            |_ -> failwithf ""
        )
        { Depth = 0; Forward = 0; Aim = 0 }
    |> fun x -> x.Depth * x.Forward

let part2 = 
    input
    |> Array.fold 
        (fun (s:State) (cmd,n) -> 
            match cmd with
            |"forward" -> { s with Forward = s.Forward + n; Depth = s.Depth + s.Aim * n }
            |"down" -> { s with Aim = s.Aim + n }
            |"up" -> { s with Aim = s.Aim - n }
            |_ -> failwithf ""
        )
        { Depth = 0; Forward = 0; Aim = 0 }
    |> fun x -> x.Depth * x.Forward
