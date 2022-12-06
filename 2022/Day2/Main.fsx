#if INTERACTIVE
#load "../Common.fsx"
#else
module Day2
#endif

open System
open AoC

type Sign = |Rock|Paper|Scissor
type Out = |Loss|Win|Draw

let ofLetter = function |"A"|"X" -> Rock |"B"|"Y" -> Paper |"C"|"Z" -> Scissor |a -> failwithf "Unsupported %s" a
let score = function |Rock -> 1 |Paper -> 2 |Scissor -> 3
let folder = __SOURCE_DIRECTORY__ + "\\"

let ofOut = function |"X" -> Loss |"Y" -> Draw |"Z" -> Win |a -> failwithf "Unsupported %s" a


let input  = load folder "input.txt" 

let parseLine (str:string) = 
    let a,b = AoC.Text.split2 " " str
    ofLetter a, ofLetter b

let outScore = function |Win -> 6 |Loss -> 0 |Draw -> 3

let part1 =     
    input 
    |> Array.map parseLine
    |> Array.map 
        (fun (o,m) ->  
            let w = 
                match o,m with 
                |Rock, Scissor -> Loss
                |Paper, Rock -> Loss
                |Scissor, Paper -> Loss 
                |Scissor, Rock -> Win
                |Rock, Paper -> Win
                |Paper, Scissor -> Win
                |_ -> Draw
            outScore w + score m
        )
    |> Array.sum

let parseLine2 (str:string) = 
    let a,b = AoC.Text.split2 " " str
    ofLetter a, ofOut b

let part2 = 
    input 
    |> Array.map parseLine2
    |> Array.map 
        (fun (o,w) ->  
            let m = 
                match o,w with 
                |Rock, Win -> Paper
                |Rock, Loss -> Scissor
                |Rock, Draw -> Rock
                |Paper, Win -> Scissor
                |Paper, Loss -> Rock
                |Paper, Draw -> Paper
                |Scissor, Win -> Rock
                |Scissor, Loss -> Paper
                |Scissor, Draw -> Scissor
            outScore w + score m
        )
    |> Array.sum


//
printfn "%i" part1
printfn "%i" part2

