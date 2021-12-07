#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day7
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\" 
let parseLine str = str
let input = load folder "input.txt" |> Array.map parseLine


let part1 = 0
let part2 = 0
