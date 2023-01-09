#if INTERACTIVE
#load "../Common.fsx"
#else
module DayT
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

//Sensor at x=20, y=1: closest beacon is at x=15, y=3
module Parse = 
    open Parser
    let parseLine (str:string) = 
        let pCord = string "x=" >>. int32 .>> string ", y=" .>>. int32
        string "Sensor at " >>. pCord .>> string ": closest beacon is at " .>>. pCord .>> eos
        |> Parser.runOrFail str

let run filename  = load folder filename |> Array.map Parse.parseLine

//let test1 = run "test1.txt"

let part1 = 0

let part2 = 0


//
printfn "%i" part1
printfn "%i" part2



let r = System.Random(1)
let src = Array.init (1048*1048*1) (fun x -> r.Next(0, 2) |> function |0 -> 'a' |_ -> 'b')  |> System.String
let fn () = 
    let p = Parser.choice2 (Parser.char 'a') (Parser.char 'b')
    let r = Parser.runOrFail src (Parser.many p)
    ()
let c = Perf.timen 10 "Parse 1MB chars" () fn


