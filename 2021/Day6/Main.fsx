#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day6
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\" 
let input = loadAll folder "input.txt" |> Text.split "," |> Array.map Int32.Parse

let mutable memo = Map.empty

///Antelet fiskar är alltid samma för fiskens internal-state och dagar kvar.
/// Givet det kan vi spara den uträkningen och återanvända tills nästa gång det behövs
let rec numFish daysLeft state =
    if daysLeft = 0 then 1L
    else
        match memo |> Map.tryFind (state,daysLeft) with
        |Some x -> x
        |None -> 
            let v = 
                if state = 0 then
                    numFish (daysLeft - 1) 6
                    + numFish (daysLeft - 1) 8
                else
                    numFish (daysLeft - 1) (state - 1)
            memo <- memo |> Map.add (state,daysLeft) v
            v
    
let solve days input = input |> Array.map (numFish days) |> Array.sum

let part1 = solve 80 input
let part2 = solve 256 input
