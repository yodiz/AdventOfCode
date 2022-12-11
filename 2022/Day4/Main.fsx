#if INTERACTIVE
#load "../Common.fsx"
#else
module DayT
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

let isRange ((x1,x2),(y1,y2)) = 
    (x1 >= y1 && x2 <= y2)
    || (y1 >= x1 && y2 <= x2)


let isOverlap ((x1,x2),(y1,y2)) = 
    x1 <= y2 && x2 >= y1


//14-83,5-14
let parseLine (str:string) = 
    let a,b = AoC.Text.split2 "," str
    let p f = let a,b = AoC.Text.split2 "-" f in a |> Parse.int32, b |> Parse.int32
    let a, b = p a , p b
    a,b

let input  = load folder "input.txt" |> Array.map parseLine

let part1 = input |> Array.filter (isRange) |> Array.length

let part2 = input |> Array.filter (isOverlap) |> Array.length


//
printfn "%i" part1
printfn "%i" part2

