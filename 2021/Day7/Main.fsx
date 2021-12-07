#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day7
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\" 
let input = loadAll folder "input.txt" |> Text.split "," |> Array.map Parse.int32

let solve input calcFuel = 
    let usageForPos pos input = input |> Array.fold (fun s x -> s + (calcFuel (abs (pos - x)))) 0
    [input |> Array.min..input |> Array.max]
    |> List.map (fun pos -> pos, usageForPos pos input)
    |> List.minBy snd
    |> snd    

//340987
let part1 = solve input id


let fuel distance= 
    let rec fuel' fuelUsed steps distance = 
        if distance = 0 then fuelUsed
        else 
            fuel' (fuelUsed + steps) (steps + 1) (distance - 1)    
    fuel' 0 1 distance

//96987874
let part2 = solve input fuel
